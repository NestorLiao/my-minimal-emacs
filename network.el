;;; -*- lexical-binding: t; -*-

(keymap-global-set "s-m" #'switch-to-gptel)
(keymap-global-set "<Tools>" #'tavily-search)

(defun switch-to-gptel()
  (interactive)
  (if (equal  (current-buffer) (gptel "*deepseek*"))
      (previous-buffer)
    (switch-to-buffer "*deepseek*" )))

(defun tavily-search-async (callback query &optional search-depth max-results exclude_domains country include_domains)
  "Perform a search using the Tavily API and return results as JSON string.
API-KEY is your Tavily API key.
QUERY is the search query string.
Optional SEARCH-DEPTH is either \"basic\" (default) or \"advanced\".
Optional MAX-RESULTS is the maximum number of results (default 5)."
  (require 'plz)
  (let* ((plz-curl-default-args (cons "-k" plz-curl-default-args))
         (url "https://api.tavily.com/search")
         (search-depth (or search-depth "advanced"))
         (max-results (or max-results 1))
         (include_anwser  t)
         (country (or country "united states"))
         (include_domains (or include_domains '("nixos.org" "freertos.org" "zephyrproject.org" "contiki-ng.org" "riot-os.org" "nuttx.apache.org" "mynewt.apache.org" "ziglang.org" "python.org" "lua.org" "elixir-lang.org" "erlang.org" "haskell.org" "cmake.org" "gnu.org" "llvm.org" "gcc.gnu.org" "qt.io" "gtk.org" "sdl.org" "libsdl.org" "qemu-project.org" "cppreference.com" "opensource.org" "ietf.org" "w3.org" "ansi.org" "iso.org" "ieee.org" "man7.org" "discourse.nixos.org" "ziggit.dev" "emacs-china.org" "lwn.net" "kernel.org" "sourceware.org" "debian.org" "archlinux.org" "github.com" "osdev.org" "opencores.org" "riscv.org" "musl-libc.org" "newlib.sourceware.org" "uclibc-ng.org" "hackaday.com" "raspberrypi.org" "arduino.cc" "espressif.com" "gentoo.org")))
         (request-data
          `(("api_key" . ,tavily-api-key)
            ("query" . ,query)
            ("search_depth" . ,search-depth)
            ("country" . ,country)
            ("include_domains" . ,include_domains)
            ("include_anwser" . ,include_anwser)
            ("exclude_domains" . ,exclude_domains)
            ("max_results" . ,max-results))))
    (plz 'post url
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-data)
      :as 'string
      :then (lambda (result) (funcall callback result)))))

(defun tavily-search (query)
  (interactive "sQuery: ")
  (tavily-search-async
   (lambda (result)
     ;; (split-window)
     (let ((buf (get-buffer-create "*tavily-search-result*")))
       (switch-to-buffer buf)
       (read-only-mode 0)
       (erase-buffer)
       (org-mode)
       (insert (tavily-result-to-org result))
       (goto-char (point-min))
       (read-only-mode 1)
       ))
   query))

(defun tavily-result-to-org (json-result)
  (let* ((data (json-read-from-string json-result))
         (results (alist-get 'results data)))
    (mapconcat (lambda (item)
                 (format "* [[%s][%s]]\n  %s"
                         (alist-get 'url item)
                         (alist-get 'title item)
                         (alist-get 'content item)))
               results
               "\n\n")))

(setq tavily-api-key
      (with-temp-buffer
        (insert-file-contents "/run/secrets/tavily_apikey")
        (buffer-string)))

(use-package gptel
  :init
  (require 'gptel-org)
  :config
  (with-eval-after-load 'gptel
    (gptel-make-tool
     :category "web"
     :name "search"
     :async t
     :function (lambda (cb keyword)
                 (tavily-search-async cb keyword "basic" 5 nil nil nil))
     :description "Search the Internet; If you used any search results, be sure to include the references in your response."
     :args (list '(:name "keyword"
                         :type string
                         :description "The keyword to search")))

    (gptel-make-tool
     :name "create_python_repl"
     :function (lambda ()
                 (run-python nil t)
                 (pop-to-buffer (python-shell-get-buffer)))
     :description "Create a new python repl for this session"
     :args nil
     :category "emacs")

    (gptel-make-tool
     :name "send_python_to_repl"
     :function (lambda (code)
                 (python-shell-send-string code))
     :args (list '(:name "code"
                         :type string
                         :description "python code to execute"))
     :description "Send some python code to the python repl for this session and execute it"
     :category "emacs")

    (gptel-make-tool
     :function (lambda (url)
                 (with-current-buffer (url-retrieve-synchronously url)
                   (goto-char (point-min)) (forward-paragraph)
                   (let ((dom (libxml-parse-html-region (point) (point-max))))
                     (run-at-time 0 nil #'kill-buffer (current-buffer))
                     (with-temp-buffer
                       (shr-insert-document dom)
                       (buffer-substring-no-properties (point-min) (point-max))))))
     :name "read_url"
     :description "Fetch and read the contents of a URL"
     :args (list '(:name "url"
                         :type "string"
                         :description "The URL to read"))
     :category "web")

    (gptel-make-tool
     :function (lambda (buffer text)
                 (with-current-buffer (get-buffer-create buffer)
                   (save-excursion
                     (goto-char (point-max))
                     (insert text)))
                 (format "Appended text to buffer %s" buffer))
     :name "append_to_buffer"
     :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
     :args (list '(:name "buffer"
                         :type "string"
                         :description "The name of the buffer to append text to.")
                 '(:name "text"
                         :type "string"
                         :description "The text to append to the buffer."))
     :category "emacs")

    ;; Message buffer logging tool
    (gptel-make-tool
     :function (lambda (text)
                 (message "%s" text)
                 (format "Message sent: %s" text))
     :name "echo_message"
     :description "Send a message to the *Messages* buffer"
     :args (list '(:name "text"
                         :type "string"
                         :description "The text to send to the messages buffer"))
     :category "emacs")

    ;; buffer retrieval tool
    (gptel-make-tool
     :function (lambda (buffer)
                 (unless (buffer-live-p (get-buffer buffer))
                   (error "Error: buffer %s is not live." buffer))
                 (with-current-buffer  buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
     :name "read_buffer"
     :description "Return the contents of an Emacs buffer"
     :args (list '(:name "buffer"
                         :type "string"
                         :description "The name of the buffer whose contents are to be retrieved"))
     :category "emacs")

    (gptel-make-tool
     :function (lambda (directory)
                 (mapconcat #'identity
                            (directory-files directory)
                            "\n"))
     :name "list_directory"
     :description "List the contents of a given directory"
     :args (list '(:name "directory"
                         :type "string"
                         :description "The path to the directory to list"))
     :category "filesystem")

    (gptel-make-tool
     :function (lambda (parent name)
                 (condition-case nil
                     (progn
                       (make-directory (expand-file-name name parent) t)
                       (format "Directory %s created/verified in %s" name parent))
                   (error (format "Error creating directory %s in %s" name parent))))
     :name "make_directory"
     :description "Create a new directory with the given name in the specified parent directory"
     :args (list '(:name "parent"
                         :type "string"
                         :description "The parent directory where the new directory should be created, e.g. /tmp")
                 '(:name "name"
                         :type "string"
                         :description "The name of the new directory to create, e.g. testdir"))
     :category "filesystem")

    (gptel-make-tool
     :function (lambda (path filename content)
                 (let ((full-path (expand-file-name filename path)))
                   (with-temp-buffer
                     (insert content)
                     (write-file full-path))
                   (format "Created file %s in %s" filename path)))
     :name "create_file"
     :description "Create a new file with the specified content"
     :args (list '(:name "path"
                         :type "string"
                         :description "The directory where to create the file")
                 '(:name "filename"
                         :type "string"
                         :description "The name of the file to create")
                 '(:name "content"
                         :type "string"
                         :description "The content to write to the file"))
     :category "filesystem")

    (gptel-make-tool
     :function (lambda (filepath)
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name filepath))
                   (buffer-string)))
     :name "read_file"
     :description "Read and display the contents of a file"
     :args (list '(:name "filepath"
                         :type "string"
                         :description "Path to the file to read.  Supports relative paths and ~."))
     :category "filesystem"))

  (defun ant/gptel-save-buffer ()
    "Save the current GPTEL buffer with the default directory
set to ~/note."
    (interactive)
    (let ((default-directory "~/talkbase/gpt/"))
      (call-interactively #'save-buffer)))

  (defun ant/gptel-load-session ()
    "Load a gptel session from ~/notes directory."
    (interactive)
    (let ((default-directory "~/.leetcode/code/"))
      (let* ((files (directory-files default-directory t ".+\\.org$"))
             (file (completing-read "Select session file: " files nil t)))
        (when file
          (find-file file)
          (gptel-mode)))))

  (setq  gptel-default-mode 'org-mode)

  (setq deepseek-api-key
        (with-temp-buffer
          (insert-file-contents "/run/secrets/deepseek_apikey")
          (buffer-string)))

  (setq gptel-model   'deepseek-chat
        gptel-backend (gptel-make-deepseek "deepseek"
                        :stream t
                        :key deepseek-api-key))

  (require 'url-util)

  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing . "You are a large language model and a writing assistant. Respond concisely.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")
          (bug . "You are a large language model and a careful programmer. The supplied code doesn't work, or contains bugs. Describe each problem using only one sentence. Provide fixes without changing the old behavior.")))
  (setq  gptel-stream nil)
  :bind (:map gptel-mode-map
              ("C-c C-d" . ant/load-gptel-directives-from-org)
              ("C-x C-s" . ant/gptel-save-buffer)))



(use-package eww
  :ensure nil
  :config
  (setq browse-url-browser-function 'eww-browse-url
        shr-use-colors nil
        eww-header-line-format ""
        shr-bullet "• "
        shr-folding-mode t
        shr-use-fonts nil
        shr-inhibit-images t
        shr-width 80
        eww-search-prefix nil
        ;; url-privacy-level '(email agent cookies lastloc)
        url-privacy-level 'none
        url-cookie-untrusted-urls '(".*")
        eww-auto-rename-buffer 'url
        eww-prompt-history '(
                             "http://zig.doc:3003/" ; "https://ziglang.org/documentation/master/"
                             "http://c.doc:3001/" ; "https://en.cppreference.com/w/c"
                             "http://cpp.doc:3002/" ; "https://en.cppreference.com/w/cpp"
                             "http://linux.doc:3000/" ;"https://www.kernel.org/doc/html/latest/"
                                        ; C-h I "https://www.gnu.org/software/emacs/manual/"
                             ))
  (defun my-eww-edit-url ()
    "Edit the current EWW URL and reload the page."
    (interactive)
    (unless (derived-mode-p 'eww-mode)
      (user-error "Not in EWW buffer"))
    (let ((current-url (plist-get eww-data :url)))
      (setq eww-data (plist-put eww-data :url
                                (read-string "Edit URL: " current-url)))
      (eww-reload)))
  (add-hook 'eww-after-render-hook 'eww-readable)
  :bind (:map eww-mode-map
              ("e" . my-eww-edit-url)))
