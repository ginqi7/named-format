;;; named-format.el --- Format by named placeholder for emacs lisp.  -*- lexical-binding: t; -*-

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

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `named-format'
;;    Format TEMPLATE by replacing named placeholder with NAME-VALUE-ALIST.
;;    Keybinding: M-x named-format
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/named-format/
;; ```
;; git clone git@github.com:ginqi7/named-format.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/named-format/")
;; (require 'named-format)
;; ```
;;

;;; Code:

(defun named-format (template name-value-alist)
  "Format TEMPLATE by replacing named placeholder with NAME-VALUE-ALIST."
  (interactive)
  (with-temp-buffer
    (insert template)
    (goto-char (point-min))
    (let ((values '())
          key)
      (while (re-search-forward "[$][{]\\(\\w+\\)}" nil t)
        (setq key (match-string 1))
        (print key)
        (setq values
              (append
               values
               (list
                (alist-get key name-value-alist nil nil 'string-equal))))
        (replace-match "%s"))
      (apply 'format (buffer-string) values))))

(defun named-format-test ()
  "Test function `named-format'."
  (let ((name-value-alist '(("name" . "workd")))
        )
    (print
     (named-format "Hello ${name}, I am ${name2}" name-value-alist))))

(provide 'named-format)
;;; named-format.el ends here
