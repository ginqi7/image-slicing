;;; image-slicing.el --- Display an image as overlays.  -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "28") (dash "2.18.0") (s "1.11.0") (f "0.20.0"))
;; Copyright (C) 2024  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/image-slice/
;; ```
;; git clone git@github.com:ginqi7/image-slice.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/image-slice/")
;; (require 'image-slicing)
;; ```
;;

;;; Code:

(require 'org-element)
(require 'image)
(require 'url-util)
(require 'cl-extra)
(require 'eww)
(require 'f)
(require 'dash)

(defcustom image-slicing-cursor-fringe-bitmaps
  '(left-fringe right-arrow warning)
  "Define the Fringe Bitmaps indicator for the cursor position."
  :group 'image-slicing
  :type '(repeat symbol))

(defcustom image-slicing-download-concurrency 20
  "Define the maximum concurrency of images download."
  :group 'image-slicing
  :type 'number)

(defcustom image-slicing-line-height-scale 2
  "Define how many line height an image slice occupies."
  :group 'image-slicing
  :type 'number)

(defcustom image-slicing-max-width 700
  "Define the maximum width of images display."
  :group 'image-slicing
  :type 'number)

(defcustom image-slicing-curl-args
  '("-s" "-L"
    ;; some server returns error unless user-agent is set to modern browser, e.g. segmentfault.com
    "-A" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.71 Safari/537.36")
  "Extra arguments passed to curl."
  :group 'image-slicing
  :type '(repeat string))

(defvar image-slicing--temporary-file-template "image-slicing-remote-images-")

(defvar image-slicing-debug-p nil)

(defvar-local image-slicing--cursor-fringe-overlay nil)

(defvar-local image-slicing--links nil)

(defvar-local image-slicing--overlay-list nil)

(defvar-local image-slicing--timer nil)

(defvar-local image-slicing-curl-buffers nil  "List of buffers used by curl.")

(defun image-slicing--async-start-process (name program finish-func &rest program-args)
  "Start the executable PROGRAM asynchronously named NAME.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory."
  (let* ((buf (generate-new-buffer (concat "*" name "*")))
         (buf-err (generate-new-buffer (concat "*" name ":err*")))
         (proc (let ((process-connection-type nil))
                 (make-process
                  :name name
                  :buffer buf
                  :stderr buf-err
                  :command (cons program (-non-nil (append program-args image-slicing-curl-args)))))))
    (set-process-sentinel
     (get-buffer-process buf-err)
     (lambda (proc _change)
       (unless (or image-slicing-debug-p (process-live-p proc))
         (kill-buffer (process-buffer proc)))))

    (set-process-sentinel
     proc
     (lambda (proc status)
       (when (string= status "finished\n")
         (with-current-buffer (process-buffer proc)
           (funcall finish-func (buffer-string))
           (unless image-slicing-debug-p
             (kill-buffer))))))
    (push (cons buf buf-err) image-slicing-curl-buffers)))

(defun image-slicing--remote-file-p (image-src)
  "Check if IMAGE-SRC is a remote file."
  (string-prefix-p "http" image-src))

(defun image-slicing-supported-url-p (url)
  "Check if URL is supported by Emacs image."
  (image-supported-file-p (url-file-extension url)))

(defun image-slicing-temporary-file (url)
  "Return temporary name to save target URL."
  (f-expand (concat image-slicing--temporary-file-template (md5 url))
            temporary-file-directory))

(defun image-slicing--download-file-if-need (image-src callback)
  "If IMAGE-SRC is a remote file, download it and run the CALLBACK function.
Otherwise, just run the CALLBACK function only."

  (let ((temp-image-file (image-slicing-temporary-file image-src)))
    (cond
     ((f-exists? image-src)
      (funcall callback image-src))
     ((f-exists? temp-image-file)
      (funcall callback temp-image-file))
     ((image-slicing--remote-file-p image-src)
      (image-slicing--async-start-process
       (concat "curl-" (buffer-name))
       "curl"
       (lambda (_) (funcall callback temp-image-file))
       image-src
       ;; request might be denied by some servers without referer section in http header.
       (if (equal major-mode 'eww-mode)
           (concat "-e " (plist-get eww-data :url)))
       "-o"
       temp-image-file))
     (t
      (message "file %s not loaded..." image-src)))))

(defun image-slicing-display (start end display buffer &optional before-string after-string)
  "Make an overlay from START to END in the BUFFER to show DISPLAY.
If BEFORE-STRING or AFTER-STRING not nil, put overlay before-string or
 after-string."
  (when-let* (((buffer-live-p buffer))
              (overlay (make-overlay start end buffer)))
    (add-to-list 'image-slicing--overlay-list overlay)
    (when before-string
      (overlay-put overlay 'before-string before-string))
    (when after-string
      (overlay-put overlay 'after-string after-string))
    (overlay-put overlay 'display display)
    (overlay-put overlay 'face 'default)
    (overlay-put overlay 'overlay-type "image-slicing")
    overlay))

(defun image-slicing-slice (image-src max-rows)
  "Slice IMAGE-SRC into multiple rows limited by MAX-ROWS."
  (let* ((image (image-slicing-create-image image-src))
         (image-pixel-cons (image-size image t))
         (image-pixel-h (cdr image-pixel-cons))
         (spliced-image-line-height (* image-slicing-line-height-scale (default-font-height)))
         (rows (/ image-pixel-h spliced-image-line-height))
         (rows (min max-rows rows))
         (x 0.0)
         (dx 1.0001)
         (y 0.0)
         (dy (/ 1.0001 rows))
         (sliced-images))
    (while (< y 1.0)
      (push (list (list 'slice x y dx dy) image) sliced-images)
      (setq y (+ y dy)))
    (reverse sliced-images)))

(defun image-slicing-display-file (image-file-info)
  "Display image by IMAGE-FILE-INFO."
  (save-excursion
    (let* ((begin (plist-get image-file-info :begin))
           (end (plist-get image-file-info :end))
           (buffer (plist-get image-file-info :buffer))
           (image-src (plist-get image-file-info :src))
           (new-line-str (propertize "\n" 'face 'default
                                     'line-height t))
           (line-beginning-p (progn (goto-char begin) (equal begin (line-beginning-position)))))
      (plist-put image-file-info :status "start")
      (image-slicing--download-file-if-need
       image-src
       (lambda (image)
         (when (or (image-slicing-supported-url-p image)
                   (and (executable-find "file")
                        (image-supported-file-p
                         (string-replace "image/" "."
                                         (car (string-split (shell-command-to-string
                                                             (concat "file -b --mime " image))
                                                            ";"))))))
           (unless line-beginning-p
             (image-slicing-display begin (1+ begin) "" buffer new-line-str)
             (setq begin (1+ begin)))
	   (let* ((images (image-slicing-slice image (- end begin 1)))
		  (len (length images)))
	     (dotimes (i len)
	       (image-slicing-display begin (1+ begin) (nth i images) buffer nil
				      (when (/= i (1- len)) new-line-str))
	       (setq begin (1+ begin))))
           (image-slicing-display begin end "" buffer)
           (plist-put image-file-info :status "finished")))))))

(defun image-slicing-run-tasks ()
  "Run Tasks unstarted."
  (let ((count 0)
        status)
    (dolist (link image-slicing--links)
      (setq status (plist-get link :status))
      (when (string= status "start")
        (setq count (1+ count)))
      (when (and (< count image-slicing-download-concurrency) (string= status "init"))
        (image-slicing-display-file link)
        (setq count (1+ count))))))

(defun image-slicing--create-render-timer ()
  "Create a timer to render images at regular intervals."
  (unless image-slicing--timer
    (run-at-time nil 0.5 #'image-slicing-run-tasks)))

(defun image-slicing--overlay-list-links ()
  "List all links in the current buffer."
  (let ((links '())
        (content (buffer-string))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert content)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let ((type (org-element-property :type link))
                (path (org-element-property :path link))
                (begin (org-element-property :begin link))
                (end (org-element-property :end link))
                (raw-link (org-element-property :raw-link link)))
            (when link
              (push
               (list
                :status "init"
                :buffer buffer
                :begin begin
                :end end
                :type type
                :src
                (pcase type
                  ("file" (file-truename path))
                  (_ (url-encode-url raw-link))))
               links))))))
    (reverse links)))

(defun image-slicing-unset-cursor-fringe ()
  "Unset fringe indicator of last cursor."
  (setq cursor-type t)
  (when image-slicing--cursor-fringe-overlay
    (delete-overlay image-slicing--cursor-fringe-overlay)
    (setq image-slicing--cursor-fringe-overlay nil)))

(defun image-slicing-set-cursor-fringe ()
  "Set an fringe indicator for the cursor position."
  (image-slicing-unset-cursor-fringe)
  (setq cursor-type nil)
  (when image-slicing-cursor-fringe-bitmaps
    (let ((ov (make-overlay (point) (point))))
      (overlay-put ov 'before-string
                   (propertize "." 'display image-slicing-cursor-fringe-bitmaps))
      (setq image-slicing--cursor-fringe-overlay ov))))

(defun image-slicing-clear ()
  "Clear related overlays and some related variables."
  (interactive)
  (setq image-slicing--links nil)
  (when image-slicing--timer
    (cancel-timer image-slicing--timer)
    (setq image-slicing--timer nil))
  (mapc #'delete-overlay image-slicing--overlay-list)
  (setq image-slicing--overlay-list nil)

  (let ((kill-buffer-query-functions nil)
        (it))
    (while (setq it (pop image-slicing-curl-buffers))
      (when-let* ((process (get-buffer-process (car it)))
                  (process-live-p process))
        (kill-process process))
      (kill-buffer (car it))
      (kill-buffer (cdr it))))

  (remove-hook 'post-command-hook #'image-slicing-post-command t))

(defun image-slicing-create-image (image-src)
  "Create an image object by IMAGE-SRC.
If the image width is greater than `image-slicing-max-width`, scale it down."
  (let* ((image (create-image image-src))
         (image-pixel-cons (image-size image t))
         (image-pixel-w (car image-pixel-cons)))
    (image-flush image)
    (when (> image-pixel-w image-slicing-max-width)
      (setf (image-property image :width) image-slicing-max-width))
    image))

(defun image-slicing-post-command ()
  "Handle cursor visibility in eww-mode when over image slicing overlays.
This function is installed on `post-command-hook'."
  (when (bound-and-true-p image-slicing-mode)
    (if (cl-some (lambda (overlay)
                   (string-equal (overlay-get overlay 'overlay-type) "image-slicing"))
                 (overlays-at (point)))
        (image-slicing-set-cursor-fringe)
      (image-slicing-unset-cursor-fringe))))


(defun image-slicing-render-buffer ()
  "Auto image overlay."
  (image-slicing-clear)
  (setq image-slicing--links (image-slicing--overlay-list-links))
  (setq image-slicing--timer (image-slicing--create-render-timer))

  (add-hook 'post-command-hook #'image-slicing-post-command nil t))

(defun image-slicing-tag-img (dom &optional url)
  "Parse img DOM."
  (let ((url (string-trim-right (shr-expand-url (or url (shr--preferred-image dom))) ","))
        (output-file))
    (cond ((s-starts-with? "http" url) (insert (format "[[%s]]" url)) t)
          ((s-starts-with? "data:image" url)
           (setq output-file
                 (make-temp-file
                  "image-slicing-" nil
                  (concat "." (image-slicing-base64-image-type url))))
           (image-slicing-save-base64-image-from-source url output-file)
           (insert (format "[[file:%s]]" output-file))))))

(defun image-slicing-base64-image-type (source)
  "Get base64 image type from SOURCE."
  (when (string-match "^data:image/\\([^;]+\\);base64," source)
    (match-string 1 source)))

(defun image-slicing-save-base64-image-from-source (source output-file)
  "Save base64 image SOURCE to OUTPUT-FILE."
  (let* ((base64-string
          (replace-regexp-in-string "^data:image/[^;]+;base64," "" source))
         (binary-data
          (base64-decode-string base64-string)))
    ;; Write the decoded binary data to the output file
    (with-temp-file output-file
      (insert binary-data)
      (save-buffer)
      (message "Image saved to %s" output-file))))


(define-minor-mode image-slicing-mode
  "A minor mode that show image overlay."
  :init-value nil
  :global nil
  (if (not image-slicing-mode)
      (image-slicing-clear)
    (image-slicing-render-buffer)))

(autoload 'elfeed-show-refresh "elfeed-show.el")
(defun shr/toggle-image-slicing (&optional enable)
  "Toggle image-slicing then refresh current buffer."
  (interactive)
  (if (or enable (not (member #'image-slicing-mode eww-after-render-hook)))
      (progn
        (advice-add 'shr-tag-img :before-until #'image-slicing-tag-img)
        (add-hook 'eww-after-render-hook #'image-slicing-mode)
        (message "image-slicing-mode is ON."))
    (advice-remove 'shr-tag-img  #'image-slicing-tag-img)
    (remove-hook 'eww-after-render-hook #'image-slicing-mode)
    (message "image-slicing-mode is OFF."))
  (pcase major-mode
    ('eww-mode (eww-reload))
    ('elfeed-show-mode (elfeed-show-refresh))
    (_ nil)))


(provide 'image-slicing)
;;; image-slicing.el ends here
