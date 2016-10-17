
;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (bar) (setq g 1)))
(setq f (create foo))
(send f 'bar)
(message_dispatch)
g ;=> 1


;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (bar) (setq g (+ g 1))))
(setq f (create foo))
(send f 'bar)
(send f 'bar)
(send f 'bar)
(message_dispatch)
g ;=> 3


;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (bar) (setq g (+ g 1))))
(setq f (create foo))
(setq ff (create foo))
(send f 'bar)
(send ff 'bar)
(send f 'bar)
(send ff 'bar)
(message_dispatch)
g ;=> 4


;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (bar) (setq g (+ g 1))))
(setq f (create foo))
(setq ff (create foo))
(send f 'bar)
(send ff 'bar)
(send f 'bar)
(send ff 'bar)
(message_dispatch)
(message_dispatch)
g ;=> 4

;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (s) (setq g (+ g 1))))
(defcomp bar ()
    (receive (s) (setq g (+ g 10))))
(setq f (create foo))
(setq b (create bar))
(send f 's)
(send b 's)
(send f 's)
(send b 's)
(message_dispatch)
(message_dispatch)
g ;=> 22

;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (a) (setq g (+ g 1)))
    (receive (b) (setq g (+ g 10))))
(setq f (create foo))
(send f 'a)
(send f 'b)
g ;=> 0
(send f 'a)
(send f 'b)
(message_dispatch)
(message_dispatch)
g ;=> 22
(send f 'a)
(send f 'b)
(send f 'a)
(send f 'b)
(send f 'a)
(send f 'b)
(message_dispatch)
g ;=> 55


;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (a) (setq g (+ g 1)))
    (receive (b) (setq g (+ g 10))))
(setq f (create foo))
(broadcast 'a)
(message_dispatch)
g ;=> 1

;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (a) (setq g (+ g 1)))
    (receive (b) (setq g (+ g 10))))
(setq f (create foo))
(broadcast 'a)
(broadcast 'b)
g ;=> 0
(broadcast 'a)
(broadcast 'b)
(message_dispatch)
(message_dispatch)
g ;=> 22
(broadcast 'a)
(broadcast 'b)
(broadcast 'a)
(broadcast 'b)
(broadcast 'a)
(broadcast 'b)
(message_dispatch)
g ;=> 55


;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (receive (a) (setq g (+ g 1))))
(defcomp bar ()
    (receive (a) (setq g (+ g 10))))
(setq f (create foo))
(setq b (create bar))
(broadcast 'a)
g ;=> 0
(broadcast 'a)
(message_dispatch)
g ;=> 22
(broadcast 'a)
(broadcast 'a)
(message_dispatch)
(message_dispatch)
g ;=> 44
