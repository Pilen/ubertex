
;;;;TEST;;;;
(force_frame 1000)
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 100 200 frames))
(funcall e) ;=> 100
(force_frame)
(funcall e) ;=> 133
(force_frame)
(funcall e) ;=> 166
(force_frame)
(funcall e) ;=> 200
(force_frame)
(funcall e) ;=> 200

;;;;TEST;;;;
(force_frame 1000)
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 100.0 200.0 frames))
(funcall e) ;=> 100.0
(force_frame)
(funcall e) ;=> 133.333333
(force_frame)
(funcall e) ;=> 166.666667
(force_frame)
(funcall e) ;=> 200.0
(force_frame)
(funcall e) ;=> 200.0

;;;;TEST;;;;
(force_frame 1000)
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 100.0 200 frames))
(funcall e) ;=> 100.0
(force_frame)
(funcall e) ;=> 133.333333
(force_frame)
(funcall e) ;=> 166.666667
(force_frame)
(funcall e) ;=> 200.0
(force_frame)
(funcall e) ;=> 200.0



;;;;TEST;;;;
;;ignore;;
;;(if (> (frame) 3) 12 (+ (* (/ (- (frame) 0) 3.0) 2.0) 10))
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 10 12 frames))
(funcall e) ;=> 10.0
(force_frame)
(funcall e) ;=> 11.0
(force_frame)
(funcall e) ;=> 12.0
