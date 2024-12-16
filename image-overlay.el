;;; image-overlay.el --- Display an image as overlays.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Qiqi Jin

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
;; ~/.emacs.d/jin-emacs/
;; ```
;; git clone git@github.com:ginqi7/jin-emacs.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/jin-emacs/")
;; (require 'image-overlay)
;; ```
;;

;;; Code:

(require 'org-element)
(require 'image-mode)

(defvar image-overlay-format "/image_overlay_%04d.png")

(defvar image-overlay-line-height-offset 10)

(defvar image-overlay-list nil)

(defvar image-overlay-show-width 700)

(defvar image-overlay-temporary-directory "image-overlay-split-images-")

(defvar image-overlay-url-match-regexp ".*\\.\\(png\\|jpg\\|jpeg\\|drawio\\|svg\\)")

(defun drawio2image (path)
  "Convert drawio PATH to image."
  (let* ((drawio-exe "/Applications/draw.io.app/Contents/MacOS/draw.io")
         (file-name (expand-file-name path))
         (target-image (make-temp-file "drawio-" nil ".png"))
         (cmd
          (format "%s %s -x -f png -o %s" drawio-exe file-name target-image)))
    ;; (print cmd)
    (shell-command cmd nil nil)
    target-image))

(defun image-overlay--async-start-process (name program finish-func &rest program-args)
  "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
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
                  :command (cons program program-args)
                  :noquery async-process-noquery-on-exit))))
    (set-process-sentinel
     (get-buffer-process buf-err)
     (lambda (proc _change)
       (unless (process-live-p proc)
         (kill-buffer (process-buffer proc)))))

    (set-process-sentinel
     proc
     (lambda (proc status)
       (when (string= status "finished\n")
         (with-current-buffer (process-buffer proc)
           (funcall finish-func (string-trim (buffer-string)))
           (kill-buffer)))))))

(defun image-overlay--compute--image-split-info (image-file)
  "Compute image split info for IMAGE-FILE."
  (let* ((image (create-image image-file 'png))
         (line-height
          (+ (line-pixel-height) image-overlay-line-height-offset))
         (image-width (car (image-size image)))
         (image-height (cdr (image-size image)))
         (image-show-height
          (* image-height (/ image-overlay-show-width image-width)))
         (numbers (+ (/ image-show-height line-height) 5))
         (proportion (/ 100.0 numbers)))
    (list :show-height line-height
          :proportion proportion
          :show-width image-overlay-show-width)))

(defun image-overlay--create-image (file width height)
  "Create an image with specified FILE , WIDTH and HEIGHT."
  (create-image file 'png nil :width width :height height))

(defun image-overlay--split-image-with-imagemagick (image-file proportion output-directory callback)
  "Split the given IMAGE-FILE with PROPORTION.
to OUTPUT-DIRECTORY using imagemagick.
when process finish call CALLBACK function."
  (image-overlay--async-start-process
   "magick"
   "magick"
   (lambda (data)
     ;; list output files: filter out the files that begin with a dot.
     (funcall callback
              (directory-files output-directory t "^[^.].*")))
   image-file
   "-crop"
   (concat "100%x" (number-to-string proportion) "%")
   "+repage"
   "+adjoin"
   (concat output-directory image-overlay-format)))

(defun image-overlay--split-image-horizontally (image-file callback)
  "Split the given IMAGE-FILE horizontally.
When process is finished, call CALLBACK function."
  (let* ((image-split-info
          (image-overlay--compute--image-split-info image-file))
         (proportion (plist-get image-split-info :proportion))
         (show-width (plist-get image-split-info :show-width))
         (show-height (plist-get image-split-info :show-height))
         (temporary-file-directory
          (make-temp-file image-overlay-temporary-directory t)))
    (image-overlay--split-image-with-imagemagick
     image-file
     proportion
     temporary-file-directory
     (lambda (files)
       (funcall callback
        (mapcar
         (lambda (file)
           (image-overlay--create-image file show-width show-height))
         files))))))

(defun image-overlay-clear ()
  "Clear overlays and placeholder."
  (interactive)
  (mapc #'delete-overlay image-overlay-list)
  (setq image-overlay-list nil))

(defun image-overlay--remove-other-overlay (pos)
  (dolist (ov (overlays-at pos))
    (delete-overlay ov)))

(defun image-overlay-display (start end display &optional buffer)
  "Make an overlay from START to END in the BUFFER to show DISPLAY."
  (image-overlay--remove-other-overlay start)
  (let ((overlay
         (make-overlay start end buffer)))
    (add-to-list 'image-overlay-list overlay)
    (overlay-put overlay 'display display)
    (overlay-put overlay 'face 'default)
    overlay))

(defun image-overlay-display-file (image-file-info)
  "Display image by IMAGE-FILE-INFO."
  (save-excursion
    (let ((begin (plist-get image-file-info :begin))
          (end (plist-get image-file-info :end))
          (buffer (current-buffer))
          (file (plist-get image-file-info :src)))
      (image-overlay-display begin end "" buffer)
      (image-overlay--split-image-horizontally file
       (lambda (images)
         (cl-loop for image in images
                  for index from 0
                  do
                  (when (< begin end)
                   (image-overlay-display begin (1+ begin) (nth index images) buffer)
                   (setq begin (1+ begin))
                   (image-overlay-display begin (1+ begin) "\n" buffer)
                   (setq begin (1+ begin))))))
      (image-overlay-display begin end "" buffer))))

(defun image-overlay-list-links ()
  "List all links in the current buffer."
  (let ((links '())
        (content (buffer-string)))
    (with-temp-buffer
      (insert content)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let ((type (org-element-property :type link))
                (path (org-element-property :path link))
                (begin (org-element-property :begin link))
                (end (org-element-property :end link))
                (raw-link (org-element-property :raw-link link)))
            (when (and link (string-match-p image-overlay-url-match-regexp raw-link))
              (push
               (list
                :begin begin
                :end end
                :type type
                :src
                (pcase type
                  ("file" (file-truename path))
                  (_ raw-link)))
               links))))))
    links))

(defun image-overlay-render-buffer ()
  "Auto image overlay."
  (dolist (file-info (image-overlay-list-links))
    (image-overlay-display-file file-info)))

(define-minor-mode image-overlay-mode
  "A minor mode that show image overlay."
  :init-value nil
  :global nil
  (if (not image-overlay-mode)
      (image-overlay-clear)
    (image-overlay-render-buffer)))

(provide 'image-overlay)
;;; image-overlay.el ends here
