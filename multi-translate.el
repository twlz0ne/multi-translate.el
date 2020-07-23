;;; multi-translate.el --- Multiple translate -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/07/10
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (bing-dict "20200216.110") (google-translate "0.12") (youdao-dictionary "0.4"))
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
;;      * 2020/07/22  Add support for async request (bing & youdao).
;;      * 2020/07/22  Add support for sdcv to translate word locally.
;;      * 2020/07/13  Add support for bing/google/youdao.
;;      * 2020/07/10  Initial commit.

;;; Code:

(require 'bing-dict)
(require 'google-translate)
(require 'youdao-dictionary)
(require 'sdcv)

(defface multi-translate-dictionary-header
  '((t (:inherit default
        :underline (:style line)
        :weight bold
        :extend t)))
  "Default face for dictionary header."
  :group 'multi-translate)

(defcustom multi-translate-word-backends '(sdcv)
  "A list of services for word translating."
  :type 'list
  :group 'multi-translate)

(defcustom multi-translate-sentence-backends '(youdao bing google)
  "A list of services for sentence translating."
  :type 'list
  :group 'multi-translate)

(defcustom multi-translate-enable-async-request t
  "Whether or not enable async request."
  :type 'boolean
  :group 'multi-translate)

(defvar multi-translate-result-buffer-name "*Multi Translate*"
  "Default name of translate result buffer.")

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

(defun multi-translate--insert-translation (dictionary lang-from lang-to text &optional async-p)
  (insert "\n"
          (multi-translate--dictionary-header (capitalize
                                               (format "%s" dictionary)))
          "\n")
  (let* ((fun (format "multi-translate--%s-translation" dictionary))
         (trans (funcall (intern fun) lang-from lang-to text async-p)))
    (goto-char (point-max))
    (if (string-empty-p trans)
        (progn
          (insert (propertize "No results\n" 'face font-lock-warning-face))
          nil)
      (insert trans)
      t)))

;;; Async helpers

(defun multi-translate-result-placeholder ()
  "Generate placeholder of translate result."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))

(defun multi-translate--insert-async-result (dictionary placeholder result)
  "Replace PLACEHOLDER with RESULT of DICTIONARY in translation buffer."
  (save-excursion
    (let ((inhibit-read-only t))
      (with-current-buffer multi-translate-result-buffer-name
        (goto-char (point-min))
        (if (re-search-forward placeholder nil t)
            (replace-match (string-trim result))
          (message "Result placeholder(%s) of %s not found!"
                   placeholder
                   dictionary))))))

(defun multi-translate--bing-dict-brief (word &optional callback)
  "Return the translation of WORD from Bing.

This function is mainly taken from `bing-dict-brief'."
  (and bing-dict-cache-auto-save
       (not bing-dict--cache)
       (bing-dict--cache-load))

  (let ((cached-result (and (listp bing-dict--cache)
                            (car (assoc-default word bing-dict--cache)))))
    (if cached-result
        (progn
          ;; update cached-result's time
          (setcdr (assoc-default word bing-dict--cache) (time-to-seconds))
          (message cached-result))
      (save-match-data
        (if (not callback)
            (with-current-buffer (url-retrieve-synchronously
                                  (concat bing-dict--base-url
                                          (url-hexify-string word))
                                  t t)
              (bing-dict-brief-cb nil (decode-coding-string word 'utf-8)))
          (url-retrieve (concat bing-dict--base-url
                                (url-hexify-string word))
                        callback
                        `(,(decode-coding-string word 'utf-8))
                        t
                        t))))))

;;; Dictionary functions

(defun multi-translate--youdao-translation (lang-from lang-to text &optional async-p)
  (let* ((youdao-dictionary-from lang-from)
         (youdao-dictionary-to lang-to)
         (placeholder (when async-p (multi-translate-result-placeholder)))
         (translation
          (if async-p
              (youdao-dictionary--request
               text
               (when async-p
                 `(lambda (_status)
                    (multi-translate--insert-async-result
                     'youdao
                     ,placeholder
                     (multi-translate--strip-youdao-sentence-translation
                      (youdao-dictionary--format-result
                       (youdao-dictionary--parse-response)))))))
            (youdao-dictionary--format-result
             (youdao-dictionary--request text)))))
    (if placeholder
        (concat placeholder "\n")
      (concat
       (or (multi-translate--strip-youdao-sentence-translation translation)
           translation)))))

(defun multi-translate--bing-translation (lang-from lang-to text &optional async-p)
  (let* ((placeholder (when async-p (multi-translate-result-placeholder)))
         (translation
          (multi-translate--bing-dict-brief
           text
           (when async-p
             `(lambda (status keyword)
                (let ((result (bing-dict-brief-cb status keyword)))
                  (multi-translate--insert-async-result
                   'bing
                   ,placeholder
                   (if (string-match-p "Machine translation: " result)
                       (multi-translate--strip-bing-sentence-translation result ,text)
                     (multi-translate--format-bing-word-translation result)))))))))
    (if placeholder
        (concat placeholder "\n")
      (if (string= "No results" translation)
          ""
        (concat
         (if (string-match-p "Machine translation: " translation)
             (multi-translate--strip-bing-sentence-translation translation text)
           (multi-translate--format-bing-word-translation translation))
         "\n")))))

(defun multi-translate--google-translation (lang-from lang-to text &optional _async-p)
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

(defun multi-translate--sdcv-translation (_lang-from _lang-to word &optional _async-p)
  "Return sdcv translation for WORD."
  (sdcv-filter
   (shell-command-to-string
    (mapconcat #'identity
               (cons "sdcv" (sdcv-search-with-dictionary-args
                             word
                             sdcv-dictionary-simple-list))
               " "))))

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
         (text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (word? (not (cdr (split-string text " "))))
         (buffer (or (get-buffer multi-translate-result-buffer-name)
                     (generate-new-buffer multi-translate-result-buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (multi-translate--insert-header translate-from
                                        translate-to
                                        text)
        (let ((successeds
               (cl-remove
                nil
                (mapcar (lambda (backend)
                          (multi-translate--insert-translation
                           backend
                           translate-from
                           translate-to
                           text
                           multi-translate-enable-async-request))
                        (if word?
                            multi-translate-word-backends
                          multi-translate-sentence-backends)))))
          ;; If the word backends return no results,
          ;; try the sentence backends.
          (when (and (not successeds) word?)
            (mapcar (lambda (backend)
                      (multi-translate--insert-translation
                       backend
                       translate-from
                       translate-to
                       text
                       multi-translate-enable-async-request))
                    multi-translate-sentence-backends)))
        (goto-char (point-min)))
      (view-mode 1)
      (switch-to-buffer-other-window buffer))))

(provide 'multi-translate)

;;; multi-translate.el ends here
