;;; tmtxt-async-tasks.el -*- lexical-binding: t; -*-

(defvar tat/window-close-delay "5"
  "The time to show the result window after the async process finish execution,
  measured in second. This is a string, so if you change this value, please set
  it as a string.")

(defvar tat/window-height 10
  "The height of the result window for the async process, measured by the number
  of lines. This is a number, so if you change this value, please set it as a
  number.")

(defvar tat/buffers-list ()
  "The list of all current running async buffers. Do not set this variable
  manually.")

(defun tat/create-window ()
  "Create a new window for displaying async process and switch to that window"
  (let ((window-height (- (window-total-height (frame-root-window))
                          (+ tat/window-height 1))))
    (let ((async-window
           (split-window (frame-root-window) window-height 'below)))
      ;; not allow other-window
      (set-window-parameter async-window 'no-other-window t)
      ;; return the new window
      async-window)))

(defun tat/close-window (process)
  "Close the window that contain the process"
  (let ((current-async-buffer (process-buffer process)))
    (let ((current-async-window (get-buffer-window current-async-buffer)))
      (print
       (concat
        "Process completed.\nThe window will be closed automatically in "
        tat/window-close-delay
        " seconds.")
       current-async-buffer)
      ;; remove the the buffer from the buffer list
      (setq tat/buffers-list
            (remove (buffer-name current-async-buffer) tat/buffers-list))
      (set-window-point current-async-window
                        (buffer-size current-async-buffer))
      ;; kill the buffer and window after x seconds
      (run-at-time (concat tat/window-close-delay " sec")
                   nil 'delete-window current-async-window)
      (run-at-time (concat tat/window-close-delay " sec")
                   nil 'kill-buffer current-async-buffer))))

(defun tat/execute-async (command command-name)
  "Execute the async shell command.

COMMAND: the command to execute
COMMAND-NAME: just the name for the output buffer
HANDLER-FUNCTION: the function for handling process
ARGUMENTS: the arguments for passing into handler-function
handler function must have one argument, that is the process of the the async
task.

Create a new window at the bottom, execute the command and print
the output to that window. After finish execution, print the message to that
window and close it after x seconds"
  (let ((window-before-execute (selected-window))
        (output-buffer
         (concat "*" command-name "*" " at " (current-time-string))))

    ;; make a new window
    (select-window (tat/create-window))
    ;; not allow popup
    (add-to-list 'same-window-buffer-names output-buffer)
    ;; run async command
    (async-shell-command command output-buffer)
    ;; set event handler for the async process
    (set-process-sentinel (get-buffer-process output-buffer) 'tat/close-window-handler)
    ;; add the new async buffer to the buffer list
    (add-to-list 'tat/buffers-list output-buffer)
    ;; switch the the previous window
    (select-window window-before-execute)))

(defun tat/close-window-handler (process _event)
  "Close the window"
  (when (equal (process-status process) 'exit)
    ;; get the current async buffer and window
    (tat/close-window process)))

(defun tat/execute-async-interactive (command)
  "Interactive command for executing a shell command"
  (interactive
   (list (read-from-minibuffer "Command to execute: ")))
  (tat/execute-async command "user-command"))

(defun tat/move-to-bottom-all ()
"Move the point of all current async buffers to the end.
Sometimes the points in async output buffers stop somewhere in the middle of the
buffer, not move to the end to track the progress. Activating this function to
fix it."
  (interactive)
  (dolist (buffer tat/buffers-list)
    (set-window-point (get-buffer-window buffer)
                      (buffer-size (get-buffer buffer)))))

(defun tat/kill-all ()
  "Kill all current async processes."
  (interactive)
  ;; kill all async buffer from the buffer list
  (dolist (buffer tat/buffers-list)
    ;; get the window contains the buffer
    (let ((window (get-buffer-window buffer)))
      ;; remove process sentinel
      (set-process-sentinel (get-buffer-process buffer) nil)
      ;; delete the process
      (delete-process (get-buffer-process buffer))
      ;; kill the buffer
      (kill-buffer buffer)
      ;; delete the window
      (delete-window window))
    ;; empty the list
    (setq tat/buffers-list ())))

(provide 'tmtxt-async-tasks)
