;;; unison-sync-mode-tests.el --- Tests for unison-sync-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 John Sigman

;; Author: John Sigman

;;; Code:

(require 'ert)
(require 'unison-sync-mode)

(ert-deftest unison-sync-test-command-building ()
  "Test that Unison command is built correctly."
  (let ((expected-command "unison -batch /test/dir1 /test/dir2 -auto -ignore 'Name *.tmp' -ignore 'Name *.log' -force /test/dir1"))
    (should (equal (unison-sync-test--command-builder) expected-command))))

(ert-deftest unison-sync-test-global-mode ()
  "Test global mode toggle functionality."
  (let ((unison-global-mode-enabled t))
    ;; Test disabling
    (unison-global-mode)
    (should-not unison-global-mode-enabled)
    
    ;; Test enabling
    (unison-global-mode)
    (should unison-global-mode-enabled)))

(ert-deftest unison-sync-test-auto-enable ()
  "Test that auto-enable respects global mode state."
  (let ((unison-sync-auto-enable t)
        (unison-global-mode-enabled nil)
        (unison-root1 "/test/dir1")
        (unison-root2 "/test/dir2")
        (unison-sync-mode nil))
    
    ;; Should not enable when global mode is disabled
    (unison-sync-maybe-enable)
    (should-not unison-sync-mode)
    
    ;; Should enable when global mode is enabled
    (setq unison-global-mode-enabled t)
    (unison-sync-maybe-enable)
    (should unison-sync-mode)
    
    ;; Cleanup
    (unison-sync-mode -1)))

(provide 'unison-sync-mode-tests)
;;; unison-sync-mode-tests.el ends here