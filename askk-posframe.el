;;; askk-posframe.el --- askk candidates style with posframe -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024-2025 askk-posframe contributors
;; SPDX-License-Identifier: Apache-2.0 OR GPL-3.0-or-later

;; Version: 0.1.0
;; Keywords: i18n, text
;; URL: https://github.com/synrbb/askk-posframe
;; Package-Requires: ((emacs "29.1") (posframe "1.4.4"))

;;; Commentary:

;; This package provides askk candidates style with posframe.

;;; Code:

(require 'posframe)

(defgroup askk-posframe nil
  "Options for askk-posframe.el."
  :group 'askk)

(defgroup askk-posframe-faces nil
  "Faces for `askk-posframe'."
  :group 'askk-posframe
  :group 'faces)

(defface askk-posframe-default
  '((t :inherit header-line))
  "Default face for a frame of askk-posframe.
Only :foreground and :background attributes are used.")

(defface askk-posframe-current
  '((t :inherit completions-highlight))
  "Face for a line of selected candidate.")

(defface askk-posframe-border
  '((t :inherit mode-line))
  "Face for a border of askk-posframe.
Only :background attribute is used.")

(defface askk-posframe-annotations
  '((t :inherit completions-annotations))
  "Face for an annotation.")

(defvar askk-posframe--buffer " *askk-posframe*")
(defvar askk-posframe--margin-scale .5)
(defvar askk-posframe--padding-scale 1)
(defvar askk-posframe--height 10)
(defvar askk-posframe--border-width 1)

(defvar-local askk-posframe--lines nil)

(defvar askk-posframe--methods
  (list :show #'askk-posframe--show
        :hide #'askk-posframe--hide
        :cleanup #'askk-posframe--cleanup
        :page-size 'askk-posframe--height))

;;;###autoload
(defun askk-posframe-style (method &rest args)
  (when-let* ((obj (plist-get askk-posframe--methods method)))
    (if (functionp obj) (apply obj args) (symbol-value obj))))

(defun askk-posframe--show (position candidates index)
  (let ((content (askk-posframe--make-content candidates index)))
    (with-current-buffer (get-buffer-create askk-posframe--buffer t)
      (delete-region (point-min) (point-max))
      (insert content)))

  (let* ((font-width (default-font-width))
         (margin-width (ceiling (* font-width askk-posframe--margin-scale))))
    (posframe-show
     askk-posframe--buffer
     :position position
     :max-height askk-posframe--height
     :x-pixel-offset (- 0 margin-width askk-posframe--border-width)
     :border-width askk-posframe--border-width
     :border-color (face-attribute 'askk-posframe-border
                                   :background nil t)
     :foreground-color (face-attribute 'askk-posframe-default
                                       :foreground nil t)
     :background-color (face-attribute 'askk-posframe-default
                                       :background nil t)))

  (let* ((total (length candidates))
         (last-page (/ total askk-posframe--height))
         (page (/ index askk-posframe--height))
         (scroll-count (if (and (> page 0) (= page last-page))
                           (+ (* (1- page) askk-posframe--height)
                              (% total askk-posframe--height))
                         (* page askk-posframe--height))))
    (posframe-funcall askk-posframe--buffer #'scroll-up scroll-count)))

(defun askk-posframe--hide ()
  (setq askk-posframe--lines nil)
  (posframe-hide askk-posframe--buffer))

(defun askk-posframe--cleanup ()
  (setq askk-posframe--lines nil)
  (posframe-delete-frame askk-posframe--buffer))

(defun askk-posframe--make-content (candidates index)
  (or askk-posframe--lines
      (setq askk-posframe--lines (askk-posframe--make-lines candidates)))

  (let ((i 0)
        lines)
    (dolist (line askk-posframe--lines)
      (when (= i index)
        (setq line (copy-sequence line))
        (add-face-text-property 0 (length line) 'askk-posframe-current t line))
      (push line lines)
      (setq i (1+ i)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun askk-posframe--make-lines (candidates)
  (let* ((font-width (default-font-width))
         (margin-width (ceiling (* font-width askk-posframe--margin-scale)))
         (margin (propertize " " 'display `(space :width (,margin-width))))
         (widths (askk-posframe--compute-widths candidates))
         (padding-width (if (> (cdr widths) 0)
                            (ceiling (* font-width
                                        askk-posframe--padding-scale))
                          0))
         (padding (propertize " " 'display `(space :width (,padding-width))))
         (spaces (cons
                  (propertize " " 'display
                              `(space :align-to (,(+ margin-width
                                                     (car widths)))))
                  (propertize " " 'display
                              `(space :align-to (,(+ margin-width
                                                     (car widths)
                                                     padding-width
                                                     (cdr widths)))))))
         lines)
    (dolist (cand candidates)
      (push (concat margin
                    (car cand)
                    (car spaces)
                    padding
                    (and-let* ((annotation (cdr cand)))
                      (propertize annotation
                                  'face
                                  'askk-posframe-annotations))
                    (cdr spaces)
                    margin)
            lines))
    (nreverse lines)))

(defun askk-posframe--compute-widths (candidates)
  (seq-reduce (lambda (acc cand)
                (cons
                 (max (car acc) (string-pixel-width (car cand)))
                 (max (cdr acc) (or (and-let* ((annotation (cdr cand)))
                                      (string-pixel-width annotation))
                                    0))))
              candidates
              '(0 . 0)))

(provide 'askk-posframe)
;;; askk-posframe.el ends here
