(ns goc
    (:gen-class))
(import
    '(javax.swing JFrame)
    '(java.awt Canvas Font Graphics Color Toolkit)
    '(java.awt.event ActionListener KeyListener KeyEvent))

(defn random-world []
    (loop [x 0
           y 0
           world (transient [])]
        (loop [y y
               col (transient [])]
            (if (= y 60)
                (persistent! col)
                (recur (inc y) (conj! col (if (> (rand) 0.3) false true)))))
        (if (= x 60)
            (persistent! world)
            (recur (inc x) 0 world))))

(defn update [world]
    (let [next-world (transient (vec (repeat 60 (transient (vec (repeat 60 false))))))]
        (loop [x 0]
            (let [x+ (if (= x 59) 0 (inc x))
                  x- (if (= x 0) 59 (dec x))]
            (loop [x x
                   y 0]
                (let [y+ (if (= y 59) 0 (inc y))
                      y- (if (= y 0) 59 (dec y))]
                    (let [sum (+
                             (nth (nth world y+) x-)
                             (nth (nth world y+) x )
                             (nth (nth world y+) x+)
                             (nth (nth world y ) x-)
                             (nth (nth world y ) x+)
                             (nth (nth world y-) x-)
                             (nth (nth world y-) x )
                             (nth (nth world y-) x+))
                          current (nth (nth world y) x)]
                        (assoc! (nth x next-world)
                                (cond (< sum 2) false
                                      (> sum 3) false
                                      a
                    (if (not= y 60)
                        (recur x (inc y)))))
                (if (not= x 60)
                    (recur (inc x)))))))

(defn paint-world [#^Canvas canvas world]
    (let [buffer (.getBufferStrategy canvas)
          g      (.getDrawGraphics buffer)]
        (try
            (doto g
                (.setColor Color/white)
                (.fillRect 0, 0, 600, 600)
                (.setColor Color/black))
            (loop [x 0
                   y 0
                   world world]
                (cond (first world)
                      (doto g
                          (.fillRect x, y, 10, 10)))
                (if (not (empty? world))
                    (let [row-end? (= x 600)]
                        (recur (if row-end? 0 (+ 10 x)) (if row-end? (+ 10 y) y) (rest world)))))

            (finally (.dispose g)))
        (if (not (.contentsLost buffer))
            (. buffer show)
            (.. Toolkit (getDefaultToolkit) (sync)))))

(defn -main [& args]
    (let [frame  (JFrame. "Game of Life")
          canvas (Canvas.)
          block-size 10
          width 60
          height 60
          screen-width 600
          screen-height 600
          alive-at-start 0.3]

        (doto frame
            (.setSize screen-width screen-height)
            (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
            (.setResizable false)
            (.add canvas)
            (.setVisible true))

        (doto canvas
            (.createBufferStrategy 2)
            (.setVisible true)
            (.requestFocus))

        (loop [world (random-world)]
            (paint-world canvas world)
            (Thread/sleep 10)
            (recur (update world)))))

