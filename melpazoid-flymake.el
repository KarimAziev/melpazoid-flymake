;;; melpazoid-flymake.el --- Flymake backend with melpazoid -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/melpazoid-flymake
;; Version: 0.1.0
;; Keywords: docs
;; Package-Requires: ((emacs "27.1") (flymake "1.2.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flymake backend with melpazoid

;;; Code:

(require 'flymake)
(require 'url-parse)
(require 'find-func)

(defcustom melpazoid-flymake-directory (ignore-errors
                                         (file-name-parent-directory
                                          (file-name-parent-directory
                                           (find-library-name
                                            "melpazoid"))))
  "Melpazoid directory."
  :group 'melpazoid
  :type 'directory)

(defun melpazoid-flymake-lint-util-ssh-to-https (ssh-remote)
  "Convert SSH-REMOTE to https url."
  (with-temp-buffer
    (save-excursion
      (insert ssh-remote))
    (when (re-search-forward "@" nil t 1)
      (when-let* ((beg (point))
                  (end (re-search-forward ":" nil t 1)))
        (string-trim
         (concat "https://"
                 (buffer-substring-no-properties
                  beg (1- end))
                 "/"
                 (buffer-substring-no-properties
                  end (point-max))))))))

(defun melpazoid-flymake-lint-util-remotes-alist ()
  "Return alist of remotes and associated urls (REMOTE-NAME . REMOTE-URL)."
  (when-let ((remotes
              (with-temp-buffer
                (when (= 0 (apply #'call-process "git" nil t nil
                                  '("remote" "-v")))
                  (string-trim (buffer-string))))))
    (seq-uniq
     (mapcar (lambda (l)
               (let ((parts (split-string l)))
                 (cons (car parts)
                       (cadr parts))))
             (split-string remotes "\n" t)))))



(defun melpazoid-flymake-get-recipe-plist ()
  "Return plist of current git repo as recipe :repo, :type and :host."
  (when-let* ((url (cdar (melpazoid-flymake-lint-util-remotes-alist)))
              (urlobj (url-generic-parse-url
                       (or (melpazoid-flymake-lint-util-ssh-to-https url)
                           url)))
              (host (url-host urlobj))
              (filename (url-filename urlobj)))
    (list
     :repo
     (replace-regexp-in-string
      "^/\\|[\\.]git$" "" filename)
     :type "git"
     :host
     (file-name-base host))))

(defun melpazoid-flymake--provided-feature ()
  "Return the last-provided feature name, as a string, or nil if none."
  (save-excursion
    (goto-char (point-max))
    (cond ((re-search-backward
            (rx "(provide '"
                (group (1+ (or (syntax word)
                               (syntax symbol)))))
            nil t)
           (match-string-no-properties 1))
          ((re-search-backward "(provide-me)" nil t)
           (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))


(defun melpazoid-flymake-find-package-name (directory)
  "Find provided feature name in root VC directory of DIRECTORY."
  (let* ((files (directory-files (let ((default-directory directory))
                                   (vc-root-dir))
                                 t
                                 directory-files-no-dot-files-regexp))
         (el-files (seq-filter (lambda (it)
                                 (equal (file-name-extension it) "el"))
                               files))
         (cands))
    (dolist (file el-files)
      (with-temp-buffer
        (erase-buffer)
        (let (emacs-lisp-mode-hook)
          (emacs-lisp-mode))
        (progn
          (insert-file-contents file)
          (when-let ((provided
                      (melpazoid-flymake--provided-feature)))
            (push provided cands)))))
    (if (= 1 (length cands))
        (car cands)
      (completing-read "Package name: " cands))))

(defun melpazoid-flymake-get-recipe ()
  "Return recipe for current repo."
  (let ((name (melpazoid-flymake--provided-feature))
        (recipe
         (melpazoid-flymake-get-recipe-plist)))
    (list name
          :repo (prin1-to-string (plist-get recipe :repo))
          :fetcher (plist-get recipe :host))))

(defun melpazoid-flymake-get-melpa-recipe-in-dir (directory &optional
                                                            package-name)
  "Return recipe for PACKAGE-NAME in DIRECTORY.
Recipe is a list, e.g. (PACKAGE-NAME :repo \"owner/repo\" :fetcher github)."
  (let* ((recipe
          (let ((default-directory directory))
            (melpazoid-flymake-get-recipe-plist)))
         (name (or package-name
                   (melpazoid-flymake-find-package-name directory)
                   (car (last (split-string
                               (plist-get recipe :repo) "/" t))))))
    (list (if (stringp name)
              (intern name)
            name)
          :repo (plist-get recipe :repo)
          :fetcher (intern (plist-get recipe :host)))))

(defun melpazoid-flymake-download-url (url)
  "Download URL and return stirng."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer download-buffer
          (set-buffer download-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min)
                         (point))
          (buffer-string))
      (kill-buffer download-buffer))))

;;;###autoload
(defun melpazoid-flymake-add-template (directory)
  "Download melpazoid template to .github/workflows/melpazoid.yml in DIRECTORY."
  (interactive (list (read-directory-name "Directory: ")))
  (let ((vc-root))
    (cond ((not (file-exists-p directory))
           (user-error "Directory %s doesn't exist" directory))
          ((not (setq vc-root (let ((default-directory directory))
                                (vc-root-dir))))
           (user-error "Directory %s should be a vc root" directory))
          (t
           (let ((dir
                  (expand-file-name ".github/workflows" directory)))
             (unless (file-exists-p dir)
               (make-directory dir 'parents))
             (let
                 ((melpazoid-str
                   (melpazoid-flymake-replace-template
                    (melpazoid-flymake-get-melpa-recipe-in-dir
                     vc-root)
                    nil
                    (melpazoid-flymake-download-url
                     "https://raw.githubusercontent.com/riscy/melpazoid/master/melpazoid.yml"))))
               (write-region melpazoid-str nil
                             (expand-file-name
                              "melpazoid.yml" dir))))))))

(defun melpazoid-flymake-replace-template (recipe exist-ok melpazoid-template)
  "Replace RECIPE and EXIST-OK in MELPAZOID-TEMPLATE."
  (with-temp-buffer
    (insert melpazoid-template)
    (when (re-search-backward "^\\([\s]+\\)?RECIPE:" nil t
                              1)
      (goto-char (match-end 0))
      (skip-chars-forward "\s\t")
      (delete-region (point)
                     (line-end-position))
      (insert (prin1-to-string recipe))
      (when (and (not exist-ok)
                 (or (re-search-forward "^\\([\s]+\\)?EXIST_OK:" nil t 1)
                     (re-search-backward "^\\([\s]+\\)?EXIST_OK:" nil t 1)))
        (skip-chars-forward "\s\t")
        (let* ((beg (point))
               (end (save-excursion
                      (+ beg (skip-chars-forward "a-z")))))
          (delete-region beg end)
          (insert "false")))
      (buffer-string))))

(defvar melpazoid-flymake--process nil)

(defun melpazoid-flymake--report (stdout-buffer dir)
  "Create Flymake diag messages from contents of STDOUT-BUFFER and DIR.
Return a list of results (file line text)."
  (let ((problems))
    (with-current-buffer stdout-buffer
      (goto-char (point-max))
      (while (re-search-backward "#L\\(\\([0-9]+\\):[\s\t]?\\([^\n]+\\)\\)" nil
                                 t 1)
        (let ((line (match-string-no-properties 2))
              (text (match-string-no-properties 3)))
          (let ((file (replace-regexp-in-string "^[-][\s]+" ""
                                                (buffer-substring-no-properties
                                                 (line-beginning-position)
                                                 (point)))))
            (push (list
                   (expand-file-name file
                                     dir)
                   (string-to-number line) text)
                  problems)))))
    problems))

(defun melpazoid-flymake-eslint-make-diag (problem)
  "Make diag from PROBLEM.
PROBLEM is a list of file, line and text."
  (pcase-let* ((`(,file ,line ,text)
                problem)
               (buff (find-file-noselect file))
               (`(,beg . ,end)
                (flymake-diag-region buff line)))
    (flymake-make-diagnostic
     buff
     beg
     end
     :error
     text)))

(defun melpazoid-flymake-report (callback &rest _)
  "Create melpazoid process for current buffer.
Invoke CALLBACK with flymake diagnostics."
  (when (process-live-p melpazoid-flymake--process)
    (kill-process melpazoid-flymake--process))
  (let ((process-environment
         (append process-environment
                 (list (format "RECIPE=%s"
                               (melpazoid-flymake-get-recipe))
                       (format "LOCAL_REPO=%s"
                               (shell-quote-argument default-directory)))))
        (default-directory default-directory)
        (callback callback))
    (setq melpazoid-flymake--process
          (make-process
           :name "melpazoid-flymake"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *melpazoid-flymake*")
           :command `("make"
                      "-C"
                      ,(shell-quote-argument melpazoid-flymake-directory))
           :sentinel
           (lambda (proc &rest _ignored)
             (when (and (eq 'exit (process-status proc)))
               (let* ((proc-buffer (process-buffer proc))
                      (problems (melpazoid-flymake--report
                                 proc-buffer
                                 default-directory)))
                 (funcall
                  callback
                  (mapcar #'melpazoid-flymake-eslint-make-diag
                          problems)))))))))

;;;###autoload
(defun melpazoid-flymake-enable ()
  "Enable Flymake with melpazoid-flymake backend."
  (interactive)
  (unless melpazoid-flymake-directory
    (setq melpazoid-flymake-directory
          (ignore-errors
            (file-name-parent-directory (file-name-parent-directory
                                         (find-library-name
                                          "melpazoid"))))))
  (unless melpazoid-flymake-directory
    (user-error
     "melpazoid-flymake: `melpazoid-flymake-directory' is not set"))
  (when buffer-file-name
    (add-hook 'flymake-diagnostic-functions #'melpazoid-flymake-report
              nil t)
    (unless (bound-and-true-p flymake-mode)
      (flymake-mode 1))
    (flymake-start)))

;;;###autoload
(defun melpazoid-flymake-disable ()
  "Disable Flymake backend."
  (interactive)
  (let ((fmode (bound-and-true-p flymake-mode)))
    (progn
      (when fmode
        (flymake-mode -1))
      (remove-hook 'flymake-diagnostic-functions
                   #'melpazoid-flymake-report t)
      (when fmode
        (flymake-mode 1)))))

;;;###autoload
(define-minor-mode melpazoid-flymake-mode
  "Run flymake with melpazoid-flymake backend."
  :lighter " FlyMelpazoid"
  :global nil
  (if melpazoid-flymake-mode
      (melpazoid-flymake-enable)
    (melpazoid-flymake-disable)))

(provide 'melpazoid-flymake)
;;; melpazoid-flymake.el ends here