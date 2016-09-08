;;; -*- Base: 10; Package: X-SCREEN; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-

;; not sure whether the next two blocks (ZWEI and TV) are actually needed :-/
#||
;; ZWEI
(defun create-comtab (name &optional (temporary-p nil))
  (let ((new-comtab (make-comtab name (or name "Unnamed")
				 keyboard-array (make-array '(300 16.))
				 mouse-array (make-array '(5 32.)))))
    (unless temporary-p (push new-comtab *ALL-THE-COMTABS*))
    new-comtab))

;; ZWEI
(dolist (c *all-the-comtabs*)
  (when (comtab-mouse-array c)
    (common-lisp-user::adjust-array (comtab-mouse-array c)
				    '(5 32.))))
||#

#||
;; TV
(defflavor basic-remote-mouse
	(console
	 wired-mouse
	 (max-n-buttons 5)
	 (max-size 0)
	 (last-cursor (make-array 32)))
	()
  (:required-flavors basic-mouse local-mouse-mixin)
  (:initable-instance-variables console max-n-buttons max-size wired-mouse)
  (:readable-instance-variables (mouse-max-n-buttons max-n-buttons)))
||#


;; This is definitely needed, changes are in (:button-press :button-release) only
;; X-SCREEN
(defvar *debug-mouse* nil)

(defmethod (x-console-input-process-top-level x-console) ()
  (with-x-console-restart (self)
    (unwind-protect
	(progn
	  (setq input-process *current-process*)
	  (catch-network-errors ()
	    (setf (xlib:display-error-handler display) #'x-error-handler)
	    (xlib:event-case (display :discard-p t :force-output-p nil)
	      ((:motion-notify) (x y)
	       (let ((mx x) (my y))
		 ;; Skip over any strung-together mouse motion events and
		 ;; set the mouse cursor to the most recent one.
		 (xlib:process-event
		   display
		   :force-output-p nil
		   :discard-p t
		   :peek-p t
		   :timeout 0
		   :handler #'(lambda (&key event-key x y &allow-other-keys)
				(declare (sys:downward-function))
				(cond ((eq event-key :motion-notify)
				       (setq mx x my y)
				       nil)
				      (t))))
		 (when (and have-pointer-p have-focus-p)
		   (tv:remote-console-new-mouse-position mx my)))
	       nil)
	      ((:button-press :button-release) (event-key code state x y time)
               (push (list event-key code state x y time (ecase event-key
		   (:button-press (logior (lsh 1 (- code 1)) state))
		   (:button-release (logandc1 (lsh 1 (- code 1)) state)))) *debug-mouse*)
	       (tv:remote-console-new-mouse-buttons
		 (ecase event-key
		   (:button-press (logior (lsh 1 (- code 1)) state))
		   (:button-release (logandc1 (lsh 1 (- code 1)) state)))
		 x y (local-event-time-from-server-event-time time))
	       nil)
	      ((:key-press :key-release) (event-key code)
	       (let ((hard-char (dpb (ecase event-key
				       (:key-press sys:%type-key-down)
				       (:key-release sys:%type-key-up))
				     sys:%%kbd-hardware-char-opcode
				     (dpb (keycode->keynum code)
					  sys:%%kbd-hardware-char-key-number
					  0))))
		 (tv:io-buffer-put tv:keystroke-buffer hard-char t nil nil)
		 (tv:console-process-input self)
		 (when (and cli::esc-function cli::keyboard-process)
		   (process:process-wakeup cli::keyboard-process)))
	       nil)
	      ((:exposure) (count (:window in-window) x y width height)
	       (cond ((xlib:window-equal in-window window)
		      (dolist (screen tv:all-the-screens)
			(when (and (eq self (send screen :console))
				   (eq in-window (tv:screen-buffer screen)))
			  (tv:remote-console-asynchronous-refresh
			    self screen x y (+ x width) (+ y height) :wakeup (zerop count)))))
		     ((and icon-window (xlib:window-equal in-window icon-window))
		      (show-icon)))
	       nil)
	      ((:visibility-notify) (state)
	       (setf window-visibility state)
	       nil)
	      ((:mapping-notify) (request start count)
	       (xlib:mapping-notify display request start count)
	       (case request
		 (:keyboard (x-console-update-keyboard-mapping self))
		 (:modifier (x-console-update-keyboard-mapping self))
		 (:pointer))
	       nil)
	      ((:keymap-notify) (keymap)
	       (update-key-states keymap)
	       nil)
	      ((:configure-notify) ((:window in-window) x y width height (:border-width bw))
	       (cond ((xlib:window-equal in-window window)
		      (setq window-x x window-y y border-width bw)
		      (setq border-width bw)
		      (setq user-specified-size-p t user-specified-position-p t)
		      (unless (and (= window-width width) (= window-height height))
			(setf window-width width window-height height)
			(tv:remote-console-note-size-changed self)))
		     ((and icon-window (xlib:window-equal in-window icon-window))
		      (setf icon-window-width width)
		      (setf icon-window-height height)))
	       nil)
	      ((:enter-notify) (x y focus-p)
	       (setf have-pointer-p t)
	       (when (and focus-p (not have-focus-p))
		 (setf have-focus-p :from-enter-notify))
	       (when have-focus-p
		 (tv:remote-console-new-mouse-position x y)
		 (when pending-warp-pointer
		   (apply #'x-console-warp-pointer self (shiftf pending-warp-pointer nil))))
	       nil)
	      ((:leave-notify) (focus-p)
	       (setf have-pointer-p nil)
	       (when (or (not focus-p) (eq have-focus-p :from-enter-notify))
		 (setf have-focus-p nil))
	       (tv:remote-console-new-mouse-position -1 -1)
	       nil)
	      ((:focus-in) ()
	       (setf have-focus-p t)
	       (when (and have-pointer-p pending-warp-pointer)
		 (apply #'x-console-warp-pointer self (shiftf pending-warp-pointer nil)))
	       nil)
	      ((:focus-out) ()
	       (setf have-focus-p nil)
	       (tv:remote-console-new-mouse-position -1 -1)
	       nil)
	      ((:map-notify) ((:window in-window))
	       (cond ((xlib:window-equal in-window window)
		      (setf initial-state :normal)
		      (tv:remote-console-new-mouse-position -1 -1)))
	       nil)
	      ((:unmap-notify) ((:window in-window))
	       (cond ((xlib:window-equal in-window window)
		      (setf initial-state :iconic)
		      (tv:remote-console-new-mouse-position -1 -1)))
	       nil)
	      ((:client-message) (format data)
	       (when remote-program
		 (x-screen-program-client-message-event remote-program format data))
	       nil)	
      ))
	  (setf (xlib:display-error-handler display) #'xlib:default-error-handler)
	  (x-console-shutdown self))
      (store-conditional (locf input-process) *current-process* nil))))

