;;; multi-translate.el --- Multiple translate -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/07/10
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/multi-translate.el
;; Keywords: tool

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

;; Translate word or region at point with multiple translation services.
;;
;; Currently supports bing/google/youdao.
;;
;; See README.md for more information.

;;; Change Log:

;;  v0.1.0
;;      * 2020/07/13  Add support for bing/google/youdao.
;;      * 2020/07/10  Initial commit.

;;; Code:

(require 'bing-dict)
(require 'google-translate)
(require 'youdao-dictionary)

(defface multi-translate-dictionary-header
  '((t (:inherit default
        :underline (:style line)
        :weight bold
        :extend t)))
  "Default face for dictionary header."
  :group 'multi-translate)

(defvar multi-translate-from "en"
  "Default source language.")

(defvar multi-translate-to "zh-CN"
  "Default target language.")

(defun multi-translate--strip-youdao-sentence-translation (translation)
  (with-temp-buffer
    (insert translation)
    (goto-char (point-min))
    (if (re-search-forward "\\* Translation\n\\- " nil t)
        (buffer-substring (point) (point-max)))))

(defun multi-translate--strip-bing-sentence-translation (translation text)
  (with-temp-buffer
    (insert translation)
    (goto-char (point-min))
    (if (re-search-forward (format "Machine translation: %s -> " text) nil t)
        (buffer-substring (point) (point-max)))))

(defun multi-translate--format-bing-word-translation (translation)
  (pcase-let ((`(,word ,defs) (split-string translation ": ")))
    (if defs
        (format "%s\n%s"
                (concat word (if (string= word "Sounds like") ":") "\n")
                (let ((array (split-string defs " | ")))
                  (setf (car (last array))
                        (replace-regexp-in-string "^\\([^\s]+\\)"
                                                  "{\\1}"
                                                  (car (last array))))
                  (string-join array "\n")))
      word)))

(defun multi-translate--dictionary-header (label)
  (let ((header (concat "📖 " label "\n")))
    (put-text-property 0 (length header)
                       'face
                       'multi-translate-dictionary-header
                       header)
    header))

(defun multi-translate--insert-header (lang-from lang-to text)
  (let ((text (concat text "\n")))
    (put-text-property 0 (length lang-from)
                       'face 'font-lock-keyword-face
                       lang-from)
    (put-text-property 0 (length lang-to)
                       'face 'font-lock-keyword-face
                       lang-to)
    (put-text-property 0 (length text)
                       'face 'widget-field
                       text)
    (insert (format "Translate from ‘%s’ to ‘%s’:" lang-from lang-to)
            "\n\n"
            text)))

(defun multi-translate--insert-translation (dictionary lang-from lang-to text)
  (insert "\n"
          (multi-translate--dictionary-header (capitalize
                                               (format "%s" dictionary)))
          "\n")
  (let* ((fun (format "multi-translate--%s-translation" dictionary))
         (trans (funcall (intern fun) lang-from lang-to text)))
    (goto-char (point-max))
    (insert trans)))

;;; Dictionary functions

(defun multi-translate--youdao-translation (lang-from lang-to text)
  (let* ((youdao-dictionary-from lang-from)
         (youdao-dictionary-to lang-to)
         (translation (youdao-dictionary--format-result text)))
    (concat
     (or (multi-translate--strip-youdao-sentence-translation translation)
         translation))))

(defun multi-translate--bing-translation (lang-from lang-to text)
  (let ((translation (bing-dict-brief text t)))
    (concat
     (if (string-match-p "Machine translation: " translation)
         (multi-translate--strip-bing-sentence-translation translation text)
       (multi-translate--format-bing-word-translation translation))
     "\n")))

(defun multi-translate--google-translation (lang-from lang-to text)
  (let* ((json (google-translate-request lang-from lang-to text))
         (translation (google-translate-json-translation json))
         (detailed-translation
          (google-translate-json-detailed-translation json))
         (detailed-definition
          (google-translate-json-detailed-definition json)))
    (concat translation "\n"
            (if detailed-translation
                (concat (google-translate--detailed-translation
                         detailed-translation translation
                         "\n%s\n" "%2d. %s\n")))
            (if detailed-definition
                (concat (google-translate--detailed-definition
                         detailed-definition translation
                         "\n%s\n" "%2d. %s\n"))))))

;;;###autoload
(defun multi-translate-at-point (arg)
  "Translate word or region at point."
  (interactive "P")
  (let* ((translate-from (if arg
                             (read-string "Translate from: " multi-translate-from)
                           multi-translate-from))
         (translate-to (if arg
                           (read-string "Translate to: "
                                        (if (string-prefix-p "zh" translate-from)
                                            "en"
                                          multi-translate-to))
                         multi-translate-to))
         (bounds (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (buffer (or (get-buffer "*Multi Translate*")
                     (generate-new-buffer "*Multi Translate*"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (multi-translate--insert-header      translate-from
                                             translate-to
                                             text)
        (multi-translate--insert-translation 'youdao
                                             translate-from
                                             translate-to
                                             text)
        (multi-translate--insert-translation 'bing
                                             translate-from
                                             translate-to
                                             text)
        (multi-translate--insert-translation 'google
                                             translate-from
                                             translate-to
                                             text)
        (goto-char (point-min)))
      (view-mode 1)
      (switch-to-buffer-other-window buffer))))

(provide 'multi-translate)

;;; multi-translate.el ends here
