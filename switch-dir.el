;;; switch-dir.el --- Switch between 2 directories
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/switch-dir

;;; Commentary:

;; The project is hosted at https://github.com/sabof/switch-dir
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)

(defvar switch-dir-spec nil)

(defun switch-dir-switch (dir1 dir2 &optional ext1 ext2 create)
  (setq dir1 (or dir1 default-directory))
  (setq dir2 (or dir2 default-directory))

  (let (( file-name (expand-file-name
                     (if (eq major-mode 'dired-mode)
                         (progn
                           (setq ext1 nil)
                           (setq ext2 nil)
                           (dired-current-directory))
                       buffer-file-name))))
    (when (> (length dir2) (length dir1))
      (cl-rotatef dir1 dir2)
      (cl-rotatef ext1 ext2))
    (let (( filename1
            (ignore-errors
              (concat dir2
                      (substring file-name
                                 (length dir1)
                                 (when ext1
                                   (- -1 (length ext1))))
                      (when ext2 ".")
                      ext2
                      )))
          ( filename2
            (ignore-errors
              (concat dir1
                      (substring file-name
                                 (length dir2)
                                 (when ext2
                                   (- -1 (length ext2))))
                      (when ext1 ".")
                      ext1
                      ))))
      (cond ( (and filename1
                   (string-prefix-p dir1 file-name)
                   (or (not ext1)
                       (string-suffix-p ext1 file-name))
                   (or create
                       (file-exists-p filename1))
                   )
              (find-file filename1))
            ( (and filename2
                   (string-prefix-p dir2 file-name)
                   (or (not ext2)
                       (string-suffix-p ext2 file-name))
                   (or create
                       (file-exists-p filename2)))
              (find-file filename2))))))

(defun switch-dir-root ()
  (locate-dominating-file default-directory ".dir-locals.el"))

(cl-defun switch-dir (&optional create)
  (interactive "P")
  (unless switch-dir-spec
    (cl-return-from switch-dir))
  (cl-loop with root = (switch-dir-root)
           with buffer-ext = (file-name-extension (buffer-file-name))
           for (src test ext) in switch-dir-spec
           do (when (switch-dir-switch
                     (concat root src)
                     (concat root test)
                     (when ext
                       buffer-ext)
                     (when ext
                       (concat ext "." buffer-ext))
                     create)
                (cl-return t))))

(provide 'switch-dir)
;;; switch-dir.el ends here
