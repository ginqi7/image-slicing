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

(defvar image-overlay-line-height-offset 3)

(defvar image-overlay-show-width 700)

(defvar image-overlay-format "/image_overlay_%04d.png")

(defvar image-overlay-temporary-directory-prefix)

(defvar image-overlay-placeholder "__#_#_@@_#_#__")

(defvar image-overlay-list nil)

(defvar image-overlay-url-match-regexp ".*\\.\\(png\\|jpg\\|jpeg\\)")

(require 'image-mode)

(defun image-overlay-display (image)
  "Display an IMAGE as an overlay at POINT (or current point if nil)
with a WIDTH (or image width if nil)."
  (let ((overlay
         (make-overlay (line-beginning-position) (line-end-position))))
    (add-to-list 'image-overlay-list overlay)
    (overlay-put overlay 'display image)
    overlay))

(defun image-overlay-clear-placeholder ()
  "Clear placeholder."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward image-overlay-placeholder nil t)
      (delete-line))))

(defun image-overlay-clear ()
  "Clear overlays and placeholder."
  (interactive)
  (mapc #'delete-overlay image-overlay-list)
  (image-overlay-clear-placeholder)
  (setq image-overlay-list nil))

(defun org-at-image-url-p ()
  "Return non-nil if the current position is at an image URL."
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (raw-link (org-element-property :raw-link link))
         (path (org-element-property :path link))
         (type (org-element-property :type link)))
    (when (and link
               (string-match-p image-overlay-url-match-regexp raw-link))
      (if (string= type "file") (file-truename path) raw-link))))

(defun image-overlay-display-file (file)
  "Display FILE."
  (save-excursion
    (let ((images (image-overlay--split-image-horizontally file)))

      (dotimes (i (length images))
        (end-of-line)
        (when (equal i 0) (insert "\n"))
        (insert image-overlay-placeholder)
        (image-overlay-display (nth i images))
        (when (not (equal i (- (length images) 1)))
          (insert "\n"))))))

(defun image-overlay-display-current ()
  "Display current image."
  (interactive)
  (when-let ((image-file (org-at-image-url-p)))
    (image-overlay-display-file image-file)))

(defun open-new-empty-line-below ()
  "Open new empty-line."
  (end-of-line)
  (insert "\n "))

(defun image-overlay--split-image-with-imagemagick (image-file proportion output-directory)
  "Split the given IMAGE-FILE with PROPORTION.
to OUTPUT-DIRECTORY using imagemagick."
  (let* ((command-format "convert %s -crop %s +repage +adjoin %s")
         (cmd
          (format command-format
                  image-file
                  (concat "100%x" (number-to-string proportion) "%")
                  (concat output-directory image-overlay-format))))
    (shell-command-to-string cmd)
    ;; list output files: filter out the files that begin with a dot.
    (directory-files output-directory t "^[^.].*" )))

(defun image-overlay--create-image (file width height)
  "Create an image with specified FILE , WIDTH and HEIGHT."
  (create-image file 'imagemagick nil :width width :height height))

(defvar image-overlay-temporary-directory "image-overlay-split-images-")

(defun image-overlay--compute--image-split-info (image-file)
  "Compute image split info for IMAGE-FILE."
  (let* ((image (create-image image-file 'imagemagick))
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

(defun image-overlay--split-image-horizontally (image-file)
  "Split the given IMAGE-FILE horizontally."
  (let* ((image-split-info
          (image-overlay--compute--image-split-info image-file))
         (proportion (plist-get image-split-info :proportion ))
         (show-width (plist-get image-split-info :show-width ))
         (show-height (plist-get image-split-info :show-height ))
         (temporary-file-directory
          (make-temp-file image-overlay-temporary-directory  t))
         (splited-image-files
          (image-overlay--split-image-with-imagemagick image-file proportion temporary-file-directory)))
    (mapcar
     (lambda (file)
       (image-overlay--create-image file show-width show-height))
     splited-image-files)))

(defun image-overlay--could-render-p ()
  "Could render image in current?"
  (and
   (org-at-image-url-p)
   (not
    (save-excursion (forward-line) (image-overlay--placeholder-p)))))

(defun image-overlay--could-clear-p ()
  "Could clear image in current?"
  (not (or (org-at-image-url-p) (image-overlay--placeholder-p))))


(defun image-overlay--placeholder-p ()
  "Is there a placeholder?"
  (string=
   image-overlay-placeholder
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun auto-image-overlay ()
  "Auto image overlay."
  (when (image-overlay--could-render-p)
    (image-overlay-display-current))
  (when (image-overlay--could-clear-p) (image-overlay-clear)))

;;;###autoload
(define-minor-mode image-overlay-mode
  "A minor mode that show image overlay."
  :init-value nil
  :global nil
  (if (not image-overlay-mode)
      (progn (remove-hook 'post-command-hook 'auto-image-overlay t))
    (progn (add-hook 'post-command-hook 'auto-image-overlay nil t))))

(provide 'image-overlay)
;;; image-overlay.el ends here
