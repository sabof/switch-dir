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
  (setq dir1 (expand-file-name (or dir1 default-directory)))
  (setq dir2 (expand-file-name (or dir2 default-directory)))

  (when (> (length dir2) (length dir1))
    (cl-rotatef dir1 dir2)
    (cl-rotatef ext1 ext2))

  (when ext1
    (setq ext1 (concat "." ext1)))
  (when ext2
    (setq ext2 (concat "." ext2)))

  (let* (( file-name
           (expand-file-name
            (if (eq major-mode 'dired-mode)
                (progn
                  (setq ext1 nil)
                  (setq ext2 nil)
                  (dired-current-directory))
              buffer-file-name)))
         ( try-switch
           (lambda (dir1 dir2 ext1 ext2)
             (when (and (string-prefix-p dir1 file-name)
                        (string-suffix-p (or ext1 "") file-name))
               (let* (( candidate
                        (ignore-errors
                          (concat dir2
                                  (substring file-name
                                             (length dir1)
                                             (when ext1
                                               (- (length ext1))))
                                  ext2
                                  ))))
                 (and candidate
                      (or create (file-exists-p candidate))
                      (find-file candidate)))))))
    (or (funcall try-switch dir1 dir2 ext1 ext2)
        (funcall try-switch dir2 dir1 ext2 ext1))))

(defun switch-dir-root ()
  (locate-dominating-file default-directory ".dir-locals.el"))

(cl-defun switch-dir (&optional create)
  (interactive "P")
  (unless switch-dir-spec
    (cl-return-from switch-dir))
  (cl-loop with root = (switch-dir-root)
           for (src test src-ext test-ext) in switch-dir-spec
           do (when (switch-dir-switch
                     (concat root src)
                     (concat root test)
                     src-ext
                     test-ext
                     create)
                (cl-return t))))

(provide 'switch-dir)
;;; switch-dir.el ends here
