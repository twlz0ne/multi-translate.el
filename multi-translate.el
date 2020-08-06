;;; multi-translate.el --- Multiple translate -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/07/10
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (bing-dict "20200216.110") (google-translate "0.12") (youdao-dictionary "0.4") (sdcv "1.5.2"))
;; URL: https://github.com/twlz0ne/multi-translate.el
;; Keywords: tools

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

;; See README.md for more information.

;;; Change Log:

;;  v0.1.0
;;      * 2020/08/06  Add support for accumulating query results.
;;      * 2020/07/28  Add support for amending current query.
;;      * 2020/07/22  Add support for async request (bing & youdao).
;;      * 2020/07/22  Add support for sdcv to translate word locally.
;;      * 2020/07/13  Add support for bing/google/youdao.
;;      * 2020/07/10  Initial commit.

;;; Code:

(require 'imenu)
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

(defface multi-translate-language
  '((t (:inherit font-lock-keyword-face)))
  "Default face for source/target language."
  :group 'multi-translate)

(defface multi-translate-input-field
  '((t (:inherit widget-field)))
  "Default face for input field."
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

(defcustom multi-translate-language-pair #'multi-translate-language-detect-zh-en
  "A pair of languages.
Either a cons cell of the form ‘(\"SOURCE-LANGUAGE\" . \"TARGET-LANGUAGE\")’, or
a function that is called with a string and return a cons cell of language names."
  :group 'multi-translate
  :type '(choice
          (cons :tag "A pair of language names"
                (string :tag "Source language")
                (string :tag "Target language"))
          (function :tag "A function return a pair of language names")))

(defvar multi-translate-result-buffer-name "*Multi Translate*"
  "Default name of translate result buffer.")

(defcustom multi-translate-accumulate-results t
  "Whether or not to accumulate translate results."
  :type 'boolean
  :group 'multi-translate)

(defvar multi-translate--youdao-language-code-map
  '(("zh-CN" . "zh-CHS"))
  "Language code map of youdao dictionary.
Each item is a cons cell of the form ‘(\"GENERAL-CODE\" . \"YOUDAO-CODE\")’.")

(defvar multi-translate-log-p nil)

(defsubst multi-translate-log (format &rest args)
  (when multi-translate-log-p
    (with-current-buffer (get-buffer-create "*Multi translate log*")
      (goto-char (point-max))
      (insert (apply #'format format args) "\n"))))

(defun multi-translate-string-nil-p (&rest strings)
  "Return no-nil if there are nils or empty strings in STRINGS."
  (remove t (mapcar
             (lambda (string)
               (and (stringp string) (not (string-empty-p string))))
             strings)))

(defun multi-translate--generate-minibuffer-tooltip (key-desc-lists)
  "Generate propertized help doc KEY-DESC-LISTS from for minibuffer.

Each item of KEY-DESC-LISTS is a list of the form:

    (KEY-STR BRIEF)

or:

    ((KEY-STR1 KEY-STR2) BRIEF)"
  (string-join
   (mapcar (pcase-lambda (`(,key ,desc))
             (concat
              (if (consp key)
                  (string-join
                   (mapcar
                    (lambda (key)
                      (propertize key 'face 'error))
                    key)
                   "/")
                (propertize key 'face 'error))
              ": " desc))
           key-desc-lists)
   ", "))

(defun multi-translate--strip-youdao-translation (translation)
  (with-temp-buffer
    (insert translation)
    (goto-char (point-min))
    ;; Sentence (avoid repeating in result buffer)
    (if (re-search-forward "\\* Translation\n\\- " nil t)
        (buffer-substring (point) (point-max))
      ;; Word / compound word
      (buffer-substring (point-min) (point-max)))))

(defun multi-translate--strip-bing-sentence-translation (translation text)
  (substring translation
             (length (format "Machine translation: %s -> " text))))

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

(defvar multi-translate--section-separator (char-to-string ?\^L)
  "Default separator of sections.")

(defun multi-translate--section-separator-p ()
  "Return t if point at section separator."
  (let ((separator-line (concat "^" multi-translate--section-separator "$")))
    (or (looking-at-p separator-line)
        (looking-back separator-line 1))))

(defun multi-translate--separator-point-entered (old-pos cur-pos)
  (cond ((< old-pos cur-pos) (forward-line))
        ((> old-pos cur-pos) (forward-line -1)
         (when-let ((ov (multi-translate--folded-translation-section)))
           (goto-char (overlay-start ov))))))

(defun multi-translate--insert-section-separator ()
  "Insert separator of sections."
  (insert ?\n
          (propertize
           multi-translate--section-separator
           'face 'error
           'point-entered #'multi-translate--separator-point-entered)
          ?\n))

(defun multi-translate--insert-header (lang-from lang-to text)
  (insert
   (format "Translate from ‘%s’ to ‘%s’:"
           (propertize lang-from 'face 'multi-translate-language)
           (propertize lang-to   'face 'multi-translate-language))
   "\n\n"
   (propertize (concat text "\n") 'face 'multi-translate-input-field)))

(defun multi-translate--insert-translation (dictionary lang-from lang-to text &optional async-p)
  (insert "\n"
          (multi-translate--dictionary-header (capitalize
                                               (format "%s" dictionary)))
          "\n")
  (let* ((fun (format "multi-translate--%s-translation" dictionary))
         (result (condition-case err
                    (funcall (intern fun) lang-from lang-to text async-p)
                  (error (or (cdr err)
                             (format "error: %s" (car err)))))))
    (goto-char (point-max))
    (if (string-empty-p result)
        (progn
          (insert (propertize "No results\n" 'face font-lock-warning-face))
          nil)
      (insert result)
      t)))

(defun multi-translate-language-detect-zh-en (string)
  (let ((zh-words 0) (en-words 0))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((ch (char-to-string (char-after))))
          (cond
           ((string-match "\\cC" ch)
            (let ((start-point (point)))
              (forward-word)
              (setq zh-words (+ zh-words (- (point) start-point)))))
           ((string-match "[a-zA-Z]" ch)
            (forward-word)
            (setq en-words (1+ en-words)))
           (t
            (forward-char))))))
    (if (< en-words zh-words)
        (cons "zh-CN" "en")
      (cons "en" "zh-CN"))))

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
  (condition-case err
      (save-excursion
        (let ((inhibit-read-only t))
          (with-current-buffer multi-translate-result-buffer-name
            (goto-char (point-min))
            (if (re-search-forward placeholder nil t)
                (replace-match (string-trim result))
              (message "Async insert error [%s]: placeholder(%s) not found"
                       placeholder
                       dictionary)))))
    (error (message "Async insert error [%s]: %s" dictionary err))))

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

;;; Navigation

(defun multi-translate--beginning-of-translate-section-p ()
  "Return t if at the beginning of the translate section."
  (looking-back (concat "^" multi-translate--section-separator "\n") 1))

(defun multi-translate--end-of-translate-section-p ()
  "Return t if at the end of the translate section."
  (looking-at-p (concat "\n" multi-translate--section-separator "$")))

(defun multi-translate-beginning-of-translation-section ()
  "Backward to beginning of translation section.
Return the new point or nil if at the beginning of buffer."
  (let* ((inhibit-point-motion-hooks t)
         (regex (concat "^" multi-translate--section-separator "\n"))
         (old-pos (point))
         (new-pos
          (progn
            (when (looking-back regex 1)
              (forward-line -1))
            (if-let ((ov (multi-translate--folded-translation-section)))
                (overlay-start ov)
              (if (re-search-backward regex nil t)
                  (progn
                    (forward-line 1)
                    (point))
                (point-min))))))
    (if (> old-pos new-pos)
        (goto-char new-pos))))

(defun multi-translate-end-of-translation-section ()
  "Forward to end of translation section.
Return the new point or nil if at the end of buffer."
  (let* ((inhibit-point-motion-hooks t)
         (regex (concat "\n" multi-translate--section-separator "$"))
         (old-pos (point))
         (new-pos
          (progn
            (when (looking-at-p regex)
              (forward-line 1))
            (if-let ((ov (multi-translate--folded-translation-section)))
                (overlay-end ov)
              (if (re-search-forward regex nil t)
                  (progn
                    (forward-line -1)
                    (point))
                (point-max))))))
    (if (< old-pos new-pos)
        (goto-char new-pos))))

(defun multi-translate-next-translation-section ()
  "Goto next translation section."
  (interactive)
  (let* ((regex (concat "^" multi-translate--section-separator "$"))
         end-of-buffer)
    (when (looking-at-p regex)
      (unless (re-search-forward regex nil t)
        (setq end-of-buffer t)))
    (unless end-of-buffer
      (re-search-forward regex nil t))))

(defun multi-translate-prev-translation-section ()
  "Goto previous translation section."
  (interactive)
  (let* ((regex (concat "^" multi-translate--section-separator "$"))
         beginning-of-buffer)
    (unless (looking-at-p regex)
      (unless (re-search-backward regex nil t)
        (setq beginning-of-buffer t)))
    (unless beginning-of-buffer
      (re-search-backward regex nil t))))

(defun multi-translate-current-translation-section ()
  "Region of current translation section."
  (list
   (cond ((= (point) (point-min)) (point-min))
         ((looking-back (concat "^" multi-translate--section-separator "\n") 1) (point))
         ((looking-at-p (concat "^" multi-translate--section-separator "$"))
          (save-excursion
            (forward-line)
            (point)))
         (t (save-excursion
              (multi-translate-beginning-of-translation-section))))
   (cond ((= (point) (point-max)) (point-max))
         ((looking-at-p (concat "\n" multi-translate--section-separator "$")) (point))
         (t (save-excursion
              (multi-translate-end-of-translation-section))))))

;;; Folding

(defface multi-translate-section-fold-face
  '((t (:inherit success)))
  "Default face for folded section."
  :group 'multi-translate)

(defun multi-translate-open-translation-section (&optional ov)
  "Show the body of current translation section or folded OV."
  (interactive)
  (if ov 
      (remove-overlays (overlay-start ov) (overlay-end ov) 'invisible t)
    (let* ((section (multi-translate-current-translation-section)))
      (remove-overlays (car section) (cadr section) 'invisible t))))

(defun multi-translate-fold-translation-section ()
  "Hide the body of current translation section."
  (interactive)
  (let* ((section (multi-translate-current-translation-section))
         (query-info (save-excursion
                       (goto-char (car section))
                       (multi-translate--query-text-and-languages)))
         (o (progn
              (remove-overlays (car section) (cadr section) 'invisible t)
              (make-overlay (car section) (cadr section) nil t nil))))
    (overlay-put o 'invisible t)
    (overlay-put o 'evaporate t)
    (overlay-put o 'face 'multi-translate-section-fold-face)
    (overlay-put o 'display (propertize
                             (format "Translate from ‘%s’ to ‘%s’: %s [[...]]"
                                     (nth 1 query-info)
                                     (nth 2 query-info)
                                     (if (< 70 (length (nth 0 query-info)))
                                         (concat
                                          (substring (nth 0 query-info) 0 70)
                                          "...")
                                       (nth 0 query-info)))
                             'face 'multi-translate-section-fold-face))))

(defun multi-translate-open-all-translation-section ()
  "Unfold all translation sections."
  (interactive)
  (remove-overlays (point-min) (point-max) 'invisible t))

(defun multi-translate-fold-all-translation-section ()
  "Fold all translation sections."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (multi-translate-fold-translation-section)
    (while (multi-translate-end-of-translation-section)
      (multi-translate-fold-translation-section))))

(defun multi-translate--folded-translation-section ()
  "Return the overlay of folded translation section."
  (when-let ((ovs (or (overlays-at (point)) (overlays-at (1- (point))))))
    (car
     (cl-remove
      nil
      (mapcar
       (lambda (ov)
         (when (eq (overlay-get ov 'face) 'multi-translate-section-fold-face)
           ov))
       ovs)))))

(defun multi-translate-toggle-translation-section ()
  (interactive)
  (if-let ((ov (multi-translate--folded-translation-section)))
      (multi-translate-open-translation-section ov)
    (multi-translate-fold-translation-section)))

;;; Dictionary functions

(defun multi-translate--youdao-translation (lang-from lang-to text &optional async-p)
  (let* ((youdao-dictionary-from
          (or (assoc-default lang-from multi-translate--youdao-language-code-map)
              lang-from))
         (youdao-dictionary-to
          (or (assoc-default lang-to multi-translate--youdao-language-code-map)
              lang-to))
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
                     (multi-translate--strip-youdao-translation
                      (youdao-dictionary--format-result
                       (youdao-dictionary--parse-response)))))))
            (youdao-dictionary--format-result
             (youdao-dictionary--request text)))))
    (if placeholder
        (concat placeholder "\n")
      (multi-translate--strip-youdao-translation translation))))

(defun multi-translate--bing-translation (_lang-from _lang-to text &optional async-p)
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

;;; Imenu

(defface multi-translate-imenu-goto-item-face
  '((t (:inherit isearch)))
  "Face for imenu goto item."
  :group 'multi-translate)

(defvar multi-translate-imenu-generic-expression
  '(("Query"
     "^Translate from ‘\\(?:[^’]*\\)’ to ‘\\(?:[^’]*\\)’:\n\n\\(.*\\)"
     1)))

(defsubst multi-translate--imenu-delete-overlay (identity &optional beg end)
  "Delete overlay if IDENTITY has in BEG to END."
  (overlay-recenter (or end (point-max)))
  (mapc (lambda (ov)
          (if (overlay-get ov identity)
              (delete-overlay ov)))
        (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun multi-translate--imenu-flash-line (match-beg match-end)
  "Flash words in MATCH-BEG to MATCH-END."
  (interactive)
  (unwind-protect
      (let ((ov-last (get 'multi-translate--imenu-flash-line 'overlay))
            (ov (or (multi-translate--folded-translation-section)
                    (make-overlay match-beg match-end))))
        (when ov-last
          (delete-overlay ov-last))
        (when ov
          (overlay-put ov 'face 'multi-translate-imenu-goto-item-face)
          (overlay-put ov 'multi-translate-imenu-goto-item t)
          (put 'multi-translate--imenu-flash-line 'overlay ov)))
    (run-with-idle-timer
     0.6 nil (lambda ()
               (multi-translate--imenu-delete-overlay 'multi-translate-imenu-goto-item)))))

(defun multi-translate--imenu-goto-function (name position &rest rest)
  (funcall #'imenu-default-goto-function name position rest)
  (let ((pos (marker-position position)))
    (save-excursion
      (goto-char pos)
      (multi-translate--imenu-flash-line pos (point-at-eol)))))

(defun multi-translate--imenu-index-function ()
  "Default function to create an index alist of the multi translate buffer."
  (pcase-let ((matches nil)
              (inhibit-point-motion-hooks t)
              (`((,title ,reg ,n)) multi-translate-imenu-generic-expression))
    (goto-char (point-max))
    (while (re-search-backward reg nil t)
      (push (cons
             (match-string-no-properties n) (copy-marker (match-beginning n)))
            matches))
    (list (append (list title)
                  (if matches
                      matches
                    (list 'dummy))))))

;;; multi-translate-mode

(defvar multi-translate--query-edit-tooltip
  (concat "Action: "
          (multi-translate--generate-minibuffer-tooltip
           '(("RET"         "commit")
             (("TAB" "M-o") "change/swap language")
             ("C-g"         "abort")))))

(defvar multi-translate--input-edit-prompt
  "Translate (%S -> %S): ")

(defvar multi-translate--sl-read-prompt
  (concat "Language ("
          (propertize "source" 'face 'error)
          " -> target): "))

(defvar multi-translate--tl-read-prompt
  (concat "Language (%S -> "
          (propertize "target" 'face 'error)
          "): "))

(defun multi-translate-clean-buffer ()
  "Clean multi translate results buffer."
  (interactive)
  (if (eq major-mode 'multi-translate-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
    (message "Not a multi translate buffer.")))

(defun multi-translate--point-at-input-p ()
  (memq 'multi-translate-input-field (text-properties-at (point))))

(defun multi-translate--point-at-language-p ()
  (memq 'multi-translate-language (text-properties-at (point))))

(defun multi-translate--query-text-and-languages ()
  "Return current query info.

Return value is in the form of ‘(QUERY-TEXT SOURCE-LANG TARGET-LANG)’."
  (save-excursion
    (unless (multi-translate--beginning-of-translate-section-p)
      (multi-translate-beginning-of-translation-section))
    (let (sl tl beg end)
      (if (catch 'break
            (while t
              (cond
               ((and (not sl) (multi-translate--point-at-language-p))
                (setq sl (thing-at-point 'symbol t)))
               ((and (not tl) (multi-translate--point-at-language-p))
                (setq tl (thing-at-point 'symbol t)))
               ((and (not beg) (multi-translate--point-at-input-p))
                (setq beg (point)))
               ((and beg (not end)) (setq end (1- (point))))
               ((and beg end) (throw 'break t)))
              (condition-case _err
                  (goto-char (next-property-change (point)))
                (error 'wrong-type-argument (throw 'break nil)))))
          (list (buffer-substring-no-properties beg end) sl tl)))))

(defun multi-translate--read-query-info (&optional qtext sl tl)
  "Read query info from minibuffer.

QTEXT   default query text
SL      default source language
TL      default target language

Return value is in the form of ‘(QUERY-TEXT SOURCE-LANG TARGET-LANG).’"
  (let ((map (copy-keymap minibuffer-local-map))
        (state 'EDIT-QTEXT)
        new-sl
        new-tl
        new-text)
    (while (eq state 'EDIT-QTEXT)
      (minibuffer-with-setup-hook
          (lambda ()
            (setq state nil)
            (use-local-map map)
            (eldoc-minibuffer-message multi-translate--query-edit-tooltip)
            (define-key map
              (kbd "TAB")
              (lambda ()
                (interactive)
                (setq state 'CHANGE-LANGS)
                (exit-minibuffer)))
            (define-key map
              (kbd "M-o")
              (lambda ()
                (interactive)
                (setq sl (prog1 tl (setq tl sl)))
                (setq new-sl sl)
                (setq new-tl tl)
                (setq qtext (minibuffer-contents))
                (setq state 'EDIT-QTEXT)
                (exit-minibuffer))))
        (setq new-text (read-string
                        (format multi-translate--input-edit-prompt sl tl)
                        qtext))))
    (when (eq state 'CHANGE-LANGS)
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map map)
            (define-key map (kbd "TAB") #'exit-minibuffer)
            (define-key map (kbd "M-o") nil))
        (setq new-sl (read-string multi-translate--sl-read-prompt sl)))
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map map)
            (define-key map (kbd "TAB") #'exit-minibuffer)
            (define-key map (kbd "M-o") nil))
        (setq new-tl (read-string
                      (format multi-translate--tl-read-prompt new-sl)
                      (if (string= new-sl tl) ;; keep different
                          sl
                        tl)))))
    (list new-text new-sl new-tl)))

(defcustom multi-translate-mode-hook '()
  "Multi translate mode hook."
  :type 'hook
  :group 'multi-translate)

(defvar multi-translate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") #'multi-translate-amend-query)
    (define-key map (kbd "TAB") #'multi-translate-toggle-translation-section)
    map)
  "Keymap for Multi translate result mode.")

(define-derived-mode multi-translate-mode fundamental-mode "Multi translate"
  "Major mode for muti-translate buffer.

\\{multi-translate-mode-map}"
  (read-only-mode 1)
  (setq-local imenu-default-goto-function #'multi-translate--imenu-goto-function)
  (setq-local imenu-create-index-function #'multi-translate--imenu-index-function)
  (setq-local imenu-generic-expression multi-translate-imenu-generic-expression)
  (setq-local inhibit-point-motion-hooks nil)
  (run-hooks 'multi-translate-mode-hook))

;;;###autoload
(defun multi-translate (text &optional source-lang target-lang)
  "Translate TEXT from SOURCE-LANG to TARGET-LANG."
  (interactive
   (pcase-let ((`(,sl . ,tl)
                (if (consp multi-translate-language-pair)
                    multi-translate-language-pair
                  (funcall multi-translate-language-pair ""))))
     (multi-translate--read-query-info "" sl tl)))
  (let* ((word? (not (cdr (split-string text " "))))
         (language-pair
          (when (multi-translate-string-nil-p source-lang target-lang)
            (if (consp multi-translate-language-pair)
                multi-translate-language-pair
              (funcall multi-translate-language-pair text))))
         (source-lang (or source-lang (car language-pair)))
         (target-lang (or target-lang (cdr language-pair)))
         (buffer (or (get-buffer multi-translate-result-buffer-name)
                     (generate-new-buffer multi-translate-result-buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (inhibit-point-motion-hooks t))
        (if multi-translate-accumulate-results
            (progn
              (goto-char (point-max))
              (unless (= (point) (point-min))
                (save-excursion
                  (multi-translate-fold-translation-section))
                (multi-translate--insert-section-separator)))
          (erase-buffer))
        (multi-translate--insert-header source-lang
                                        target-lang
                                        text)
        (let ((successeds
               (cl-remove
                nil
                (mapcar (lambda (backend)
                          (multi-translate--insert-translation
                           backend
                           source-lang
                           target-lang
                           text
                           multi-translate-enable-async-request))
                        (if word?
                            multi-translate-word-backends
                          multi-translate-sentence-backends)))))
          ;; If the word backends return no results,
          ;; try the sentence backends.
          (when (and (not successeds) word?)
            (mapc (lambda (backend)
                    (multi-translate--insert-translation
                     backend
                     source-lang
                     target-lang
                     text
                     multi-translate-enable-async-request))
                  multi-translate-sentence-backends)))
        (multi-translate-beginning-of-translation-section))
      (unless (eq major-mode 'multi-translate-mode)
        (multi-translate-mode))
      (if-let ((window (get-buffer-window buffer)))
          (select-window window)
        (switch-to-buffer-other-window buffer)))))

;;;###autoload
(defun multi-translate-at-point (arg)
  "Translate word or region at point."
  (interactive "P")
  (let* ((bounds (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (language-pair
          (if (consp multi-translate-language-pair)
              multi-translate-language-pair
            (funcall multi-translate-language-pair text)))
         (source-lang (if arg
                          (read-string
                           multi-translate--sl-read-prompt (car language-pair))
                        (car language-pair)))
         (target-lang (if arg
                          (read-string
                           (format multi-translate--tl-read-prompt source-lang)
                           (if (string-prefix-p "zh" source-lang)
                               "en"
                             (cdr language-pair)))
                        (cdr language-pair))))
    (multi-translate text source-lang target-lang)))

;;;###autoload
(defun multi-translate-amend-query ()
  "Amend current query and resubmit it."
  (interactive)
  (if-let ((buffer (get-buffer multi-translate-result-buffer-name)))
      (with-current-buffer buffer
        (pcase-let* ((`(,qtext ,sl ,tl)
                      (multi-translate--query-text-and-languages))
                     (`(,new-text ,new-sl ,new-tl)
                      (multi-translate--read-query-info qtext sl tl)))
          (let ((language-pair
                 (if (multi-translate-string-nil-p new-sl new-tl)
                     (if (functionp multi-translate-language-pair)
                         (let ((auto-langs
                                (funcall multi-translate-language-pair new-text)))
                           (cons (if (string-empty-p new-sl)
                                     (car auto-langs)
                                   new-sl)
                                 (if (string-empty-p new-tl)
                                     (cdr auto-langs)
                                   new-tl)))
                       multi-translate-language-pair)
                   (cons new-sl new-tl))))
            (multi-translate new-text (car language-pair) (cdr language-pair)))))
    (message "No query is found.")))

(provide 'multi-translate)

;;; multi-translate.el ends here
