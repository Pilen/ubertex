
;;;;TEST;;;;
(force_frame 1000)
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 100 200 frames))
(funcall e) ;=> 100
(force_frame)
(funcall e) ;=> 133
(force_frame)
(funcall e) ;=> 167
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
(force_frame)
(funcall e) ;=> 200.0
(force_frame)
(funcall e) ;=> 200.0


;;;;TEST;;;;
(setq x 100)
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 'x 200 frames))
(funcall e) ;=> 100
x ;=> 100
(force_frame)
(funcall e) ;=> 133
x ;=> 133
(force_frame)
(funcall e) ;=> 167
x ;=> 167
(force_frame)
(funcall e) ;=> 200
x ;=> 200
(force_frame)
(funcall e) ;=> 200
x ;=> 200

;;;;TEST;;;;
(setq frames (* (/ 1.0 30.0) 3))
(setq e (let ((x 100)) (ease 'x 200 frames)))
(funcall e) ;=> 100
x ;=> 100
(force_frame)
(funcall e) ;=> 133
x ;=> 133
(force_frame)
(funcall e) ;=> 167
x ;=> 167
(force_frame)
(funcall e) ;=> 200
x ;=> 200
(force_frame)
(funcall e) ;=> 200
x ;=> 200


;;;;TEST;;;;
;;(if (> (frame) 3) 12 (+ (* (/ (- (frame) 0) 3.0) 2.0) 10))
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 10 12 frames))
(funcall e) ;=> 10
(force_frame)
(funcall e) ;=> 11
(force_frame)
(funcall e) ;=> 11
(force_frame)
(funcall e) ;=> 12



;;;;TEST;;;;
(if (> (frame) 30)
    600
    (let ((ratio (/ (- (frame) 0) 30.0)))
    (round (+ (* ratio 400.0) 400.0))))


(frame) ;=> 0
(setq e (ease 200 600 1.0))
(funcall e) ;=> 200
(force_frame)
(funcall e) ;=> 213
(force_frame)
(funcall e) ;=> 227
(force_frame)
(funcall e) ;=> 240
(force_frame)
(funcall e) ;=> 253
(force_frame)
(funcall e) ;=> 267
(force_frame)
(funcall e) ;=> 280
(force_frame)
(funcall e) ;=> 293
(force_frame)
(funcall e) ;=> 307
(force_frame)
(funcall e) ;=> 320
(force_frame)
(funcall e) ;=> 333
(force_frame)
(funcall e) ;=> 347
(force_frame)
(funcall e) ;=> 360
(force_frame)
(funcall e) ;=> 373
(force_frame)
(funcall e) ;=> 387
(force_frame)
(funcall e) ;=> 400
(force_frame)
(funcall e) ;=> 413
(force_frame)
(funcall e) ;=> 427
(force_frame)
(funcall e) ;=> 440
(force_frame)
(funcall e) ;=> 453
(force_frame)
(funcall e) ;=> 467
(force_frame)
(funcall e) ;=> 480
(force_frame)
(funcall e) ;=> 493
(force_frame)
(funcall e) ;=> 507
(force_frame)
(funcall e) ;=> 520
(force_frame)
(funcall e) ;=> 533
(force_frame)
(funcall e) ;=> 547
(force_frame)
(funcall e) ;=> 560
(force_frame)
(funcall e) ;=> 573
(force_frame)
(funcall e) ;=> 587
(force_frame)
(funcall e) ;=> 600
(force_frame)
(funcall e) ;=> 600











;;;;TEST;;;;
(force_frame 1000)
(setq frames (* (/ 1.0 30.0) 3))
(setq e (ease 100 200 frames 'in-quad))
(funcall e) ;=> 100
(force_frame)
(funcall e) ;=> 111
(force_frame)
(funcall e) ;=> 144
(force_frame)
(funcall e) ;=> 200
(force_frame)
(funcall e) ;=> 200
