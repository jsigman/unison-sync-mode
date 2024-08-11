(defvar unison-sync-queue nil
  "Queue of Unison sync commands to run.")

(defvar unison-sync-running nil
  "Flag to check if a Unison sync is currently running.")

(defvar-local unison-root1 nil
  "The first root directory for Unison sync.")

(defvar-local unison-root2 nil
  "The second root directory for Unison sync.")

(defvar-local unison-excluded nil
  "List of patterns to exclude in Unison sync.")

(defcustom unison-one-way-sync nil
  "Specify whether to perform one-way synchronization.
If non-nil, Unison will only propagate changes from `unison-root1` to `unison-root2`."
  :type 'boolean
  :group 'unison)

(defun unison-build-command ()
  "Build the Unison command based on directory local variables."
  (when (and unison-root1 unison-root2)
    (let ((command
           (concat
            "unison -batch " unison-root1 " " unison-root2 " -auto")))
      (dolist (pattern unison-excluded)
        (setq command (concat command " -ignore 'Name " pattern "'")))
      (when unison-one-way-sync
        (setq command (concat command " -force " unison-root1)))
      command)))

(defun unison-process-next-command ()
  "Process the next command in the queue if not currently running."
  (when (and (not unison-sync-running) unison-sync-queue)
    (let ((command (pop unison-sync-queue)))
      (unison-run-command command))))

(defun unison-run-command (command)
  "Run a Unison command."
  (let ((output-buffer (get-buffer-create "*Unison Sync*")))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Running command: %s\n\n" command)))
    (setq unison-sync-running t)
    (let ((process
           (make-process
            :name "unison-sync"
            :buffer output-buffer
            :command (list "sh" "-c" command)
            :filter
            (lambda (proc string)
              (when (buffer-live-p (process-buffer proc))
                (with-current-buffer (process-buffer proc)
                  (let ((moving (= (point) (process-mark proc))))
                    (save-excursion
                      (goto-char (process-mark proc))
                      (insert
                       (replace-regexp-in-string "\r" "\n" string))
                      (set-marker (process-mark proc) (point)))
                    (if moving
                        (goto-char (process-mark proc))))))))))
      (set-process-sentinel process 'unison-sync-sentinel))))

(defun unison-sync-sentinel (process event)
  "Handle completion of a Unison process."
  (setq unison-sync-running nil)
  (unison-process-next-command) ; Process next command in the queue
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (format "\nProcess %s %s" process event))
    (if (zerop (process-exit-status process))
        (message
         (propertize "Unison sync completed successfully"
                     'face
                     '(:foreground "green")))
      (message
       (propertize (format "Unison sync failed: %s" event)
                   'face
                   '(:foreground "red"))))))

(defun unison-sync-on-save ()
  "Queue Unison sync command on file save."
  (let ((command (unison-build-command)))
    (when command
      (push command unison-sync-queue)
      (unison-process-next-command))))

(defun unison-force-sync ()
  "Force a Unison sync with -ignorearchives flag."
  (interactive)
  (let ((command (unison-build-command)))
    (when command
      (setq command (concat command " -ignorearchives"))
      (push command unison-sync-queue)
      (unison-process-next-command))))

(define-minor-mode unison-sync-mode
  "Minor mode to sync the current project using Unison on file save."
  :lighter
  " Unison-Sync"
  (if unison-sync-mode
      (add-hook 'after-save-hook 'unison-sync-on-save nil t)
    (remove-hook 'after-save-hook 'unison-sync-on-save t)))

(defun maybe-enable-unison-sync-mode ()
  "Enable `unison-sync-mode` if `unison-root1` and `unison-root2` are set."
  (when (and unison-root1 unison-root2)
    (unison-sync-mode 1)))

(add-hook 'hack-local-variables-hook 'maybe-enable-unison-sync-mode)
