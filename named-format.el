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

(defvar named-format-match-regex "[$][{]\\(.*?\\)}")

(defun named-format-sql (template name-value-list)
  "For sql format TEMPLATE NAME-VALUE-LIST."
  (interactive)
  (named-format--1
   template
   name-value-list
   "?"))

(defun named-format--1 (template name-value-list placeholder)
  "Generate named format STRING and VALUES.
TEMPLATE is string template.
NAME-VALUE-LIST is name value list.
PLACEHOLDER is placeholder for replace."
  (with-temp-buffer
    (insert template)
    (goto-char (point-min))
    (let ((values '())
          key)
      (while (re-search-forward
              named-format-match-regex
              nil
              t)
        (setq key (match-string 1))
        (setq values
              (append
               values
               (named-format--get-value
                key
                name-value-list)))
        (replace-match placeholder))
      (list
       (substring-no-properties
        (buffer-string))
       values))))

(defun named-format (template name-value-list)
  "Format TEMPLATE by replacing named placeholder with NAME-VALUE-LIST.
Support alist or plist."
  (interactive)
  (let* ((string-with-values (named-format--1
                              template
                              name-value-list
                              "%s"))
         (string (car string-with-values))
         (values (cadr string-with-values)))
    (apply 'format string values)))


(defun named-format--get-value (key key-value-list)
  "Alist (KEY KEY-VALUE-LIST)."
  (let ((value (plist-get
                key-value-list
                (intern (format ":%s" key)))))
    (if (not value)
        (setq value
              (alist-get
               key
               key-value-list
               nil
               nil
               'string-equal)))
    (list value)))

(defun named-format-test ()
  "Test function `named-format'."
  (let ((name-value-alist '(("name" . "hello")
                            ("name2" . "workd"))))
    (print
     (named-format
      "Hello ${name}, I am ${name2}"
      name-value-alist))))

(defun named-format-test01 ()
  "Test function `named-format'."
  (let ((name-value-plist '(:name "hello" :name2 "word")))
    (print
     (named-format
      "Hello ${name}, I am ${name2}"
      name-value-plist))))

(defun named-format-test02 ()
  "Test function `named-format-sql'."
  (let ((name-value-plist '(:name "hello" :name2 "word")))
    (print
     (named-format-sql
      "Hello ${name}, I am ${name2}"
      name-value-plist))))


(provide 'named-format)
;;; named-format.el ends here
