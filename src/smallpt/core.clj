(ns smallpt.core
  (:import (java.io File)
           (java.awt Color)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO))
  (:gen-class))

(def inf 1.0e20)
(def nc 1.0)
(def nt 1.5)

(defn vec-init [^Double x ^Double y ^Double z]
  {:x x :y y :z z})

(defn vec-add [u v]
  (let [ux (u :x) uy (u :y) uz (u :z)
        vx (v :x) vy (v :y) vz (v :z)]
    {:x (+ ux vx) :y (+ uy vy) :z (+ uz vz)}))

(defn vec-sub [u v]
  (let [ux (u :x) uy (u :y) uz (u :z)
        vx (v :x) vy (v :y) vz (v :z)]
    {:x (- ux vx) :y (- uy vy) :z (- uz vz)}))

(defn vec-mult [u v]
  (let [ux (u :x) uy (u :y) uz (u :z)
        vx (v :x) vy (v :y) vz (v :z)]
    {:x (* ux vx) :y (* uy vy) :z (* uz vz)}))

(defn vec-mul [v s]
  {:x (* (v :x) s)
   :y (* (v :y) s)
   :z (* (v :z) s)})

(defn vec-dot [u v]
  (let [ux (u :x) uy (u :y) uz (u :z)
        vx (v :x) vy (v :y) vz (v :z)]
    (+ (* ux vx) (* uy vy) (* uz vz))))

(defn vec-cross [u v]
  (let [ux (u :x) uy (u :y) uz (u :z)
        vx (v :x) vy (v :y) vz (v :z)]
    {:x (- (* uy vz) (* uz vy))
     :y (- (* uz vx) (* ux vz))
     :z (- (* ux vy) (* uy vx))}))

(defn vec-norm [v]
  (let [x (v :x)
        y (v :y)
        z (v :z)
        nrm (Math/sqrt (+ (* x x) (* y y) (* z z)))]
     {:x (/ x nrm) :y (/ y nrm) :z (/ z nrm)}))

(defn ray-init [o d]
  {:org o :dir d})

(defn sphere-init [r pos emit color refl]
  {:r r :pos pos :emit emit :color color :refl refl})

(defn sphere-intersect [sphere ray]
  (let [op  (vec-sub (:pos sphere) (:org ray))
        b   (vec-dot op (:dir ray))
        r   (:r sphere)
        det (+ (- (* b b) (vec-dot op op)) (* r r))]
    (if (< det 0.0)
      inf
      (let [sqdet (Math/sqrt det)
            t1    (- b sqdet)
            t2    (+ b sqdet)
            eps   1.0e-4]
        (if (> t1 eps)
          t1
          (if (> t2 eps)
            t2
            inf))))))

(def DIFF 0)
(def SPEC 1)
(def REFR 2)

(def scene [(sphere-init 1.0e5 (vec-init (+ 1.0e5 1)   40.8 81.6)  (vec-init 0.0 0.0 0.0) (vec-init 0.75 0.25 0.25) DIFF)   ;; Left
            (sphere-init 1.0e5 (vec-init (+ -1.0e5 99) 40.8 81.6)  (vec-init 0.0 0.0 0.0) (vec-init 0.25 0.25 0.75) DIFF)   ;; Right
            (sphere-init 1.0e5 (vec-init 50.0 40.8 1.0e5)          (vec-init 0.0 0.0 0.0) (vec-init 0.75 0.75 0.75) DIFF)   ;; Back
            (sphere-init 1.0e5 (vec-init 50.0 40.8 (+ -1.0e5 170)) (vec-init 0.0 0.0 0.0) (vec-init 0.0  0.0  0.0 ) DIFF)   ;; Front
            (sphere-init 1.0e5 (vec-init 50 1.0e5 81.6)            (vec-init 0.0 0.0 0.0) (vec-init 0.75 0.75 0.75) DIFF)   ;; Bottom
            (sphere-init 1.0e5 (vec-init 50 (+ -1.0e5 81.6) 81.6)  (vec-init 0.0 0.0 0.0) (vec-init 0.75 0.75 0.75) DIFF)   ;; Top
            (sphere-init 16.5  (vec-init 27 16.5 47)               (vec-init 0.0 0.0 0.0) (vec-mul (vec-init 1 1 1) 0.999) SPEC)   ;; Mirror
            (sphere-init 16.5  (vec-init 73 16.5 78)               (vec-init 0.0 0.0 0.0) (vec-mul (vec-init 1 1 1) 0.999) REFR)   ;; Glass
            (sphere-init 600   (vec-init 50 (- 681.6 0.27) 81.6)   (vec-init 12.0 12.0 12.0)  (vec-init 0.0  0.0  0.0 ) DIFF) ])   ;; Light

(defn scene-intersect [scene ray]
  (reduce #(if (< (first %1) (first %2)) %1 %2)
    (for [[i sphere] (map-indexed vector scene)
          :let [t (sphere-intersect sphere ray)]]
          [t i])))

(defn radiance [scene ray depth]
  (let [[t, i] (scene-intersect scene ray)]
    (if (or (<= inf t) (>= depth 128))
      (vec-init 0.0 0.0 0.0)
      (let [r0   (rand)
            obj  (nth scene i)
            col  (:color obj)
            prob (max (col :x) (col :y) (col :z))]
        (if (and (>= depth 5) (>= r0 prob))
          (:emit obj)
          (let [roulette (if (< depth 5) 1 prob)
                d  (:dir ray)
                p  (vec-add (:org ray) (vec-mul d t))
                n  (vec-norm (vec-sub p (:pos obj)))
                nl (if (< (vec-dot d n) 0.0) n (vec-mul n -1))
                next-rad #(vec-add (:emit obj) (vec-mul (vec-mult col %1) (/ 1.0 roulette)))]
            (cond
              (== (:refl obj) DIFF)
                (let [r1  (* 2.0 Math/PI (rand))
                      r2  (rand)
                      r2s (Math/sqrt r2)
                      w nl
                      u (vec-norm (vec-cross (if (> (Math/abs (:x w)) 0.1) (vec-init 0.0 1.0 0.0) (vec-init 1.0 0.0 0.0)) w))
                      v (vec-norm (vec-cross w u))
                      uu (vec-mul u (* (Math/cos r1) r2s))
                      vv (vec-mul v (* (Math/sin r1) r2s))
                      ww (vec-mul w (Math/sqrt (- 1.0 r2)))
                      ndir (vec-norm (reduce vec-add [uu vv ww]))]
                  (next-rad (radiance scene (ray-init p ndir) (inc depth))))
              (== (:refl obj) SPEC)
                (let [ndir (vec-sub d (vec-mul nl (* 2.0 (vec-dot nl d))))]
                  (next-rad (radiance scene (ray-init p ndir) (inc depth))))
              (== (:refl obj) REFR)
                (let [rdir (vec-sub d (vec-mul nl (* 2.0 (vec-dot nl d))))
                      into (pos? (vec-dot n nl))
                      nnt (if into (/ nc nt) (/ nt nc))
                      ddn (vec-dot d nl)
                      cos2t (- 1.0 (* (* nnt nnt) (- 1.0 (* ddn ddn))))]
                  (if (neg? cos2t)
                    (next-rad (radiance scene (ray-init p rdir) (inc depth)))
                    (let [tdir (vec-norm (vec-sub (vec-mul d nnt) (vec-mul n (* (if into 1 -1) (-> ddn (* nnt) (+ (Math/sqrt cos2t)))))))
                          a (- nt nc)
                          b (+ nt nc)
                          R0 (/ (* a a) (* b b))
                          c (- 1.0 (if into (- ddn) (vec-dot tdir n)))
                          Re (+ R0 (* (- 1.0 R0) (Math/pow c 5.0)))
                          Tr (- 1.0 Re)
                          refp (-> Re (* 0.5) (+ 0.25))
                          r1 (rand)]
                      (if (> depth 2)
                        (if (< r1 refp)
                          (next-rad (vec-mul (radiance scene (ray-init p rdir) (inc depth)) (/ Re refp)))
                          (next-rad (vec-mul (radiance scene (ray-init p tdir) (inc depth)) (/ Tr (- 1.0 refp)))))
                        (let [rrad (radiance scene (ray-init p rdir) (inc depth))
                              trad (radiance scene (ray-init p tdir) (inc depth))]
                          (next-rad (vec-add (vec-mul rrad Re) (vec-mul trad Tr)))))))))))))))

(defn subsample []
  (let [r1 (* 2 (rand))
        r2 (* 2 (rand))
        rr1 (if (< r1 1) (- (Math/sqrt r1) 1) (- 1 (Math/sqrt (- 2 r1))))
        rr2 (if (< r2 1) (- (Math/sqrt r2) 1) (- 1 (Math/sqrt (- 2 r2))))]
    [rr1 rr2]))

(defn trace-path [scene cam x y w h spp]
  (let [cx (vec-init (/ (* w 0.5135) h) 0.0 0.0)
        cy (vec-mul (vec-norm (vec-cross cx (:dir cam))) 0.5135)
        dlt (for [i (range 4)] (subsample))
        sub (for [dx (range 2) dy (range 2)] [dx dy])
        crd (repeat 4 [x y])
        pos (for [[[x y] [sx sy] [dx dy]] (map list crd sub dlt)
                  :let [xx (- (/ (+ (/ (+ sx 0.5 dx) 2.0) x) w) 0.5)
                        yy (- (/ (+ (/ (+ sy 0.5 dy) 2.0) y) h) 0.5)]]
              [xx yy])
        dirs (for [[xx yy] pos] (reduce vec-add [(:dir cam) (vec-mul cx xx) (vec-mul cy yy)]))
        rays (for [d dirs] (ray-init (vec-add (:org cam) (vec-mul d 140.0)) (vec-norm d)))]
    (reduce vec-add (for [r rays] (vec-mul (radiance scene r 0) (* 0.25))))))

(defmacro toInt [x]
  `(-> ~x (max 0.0) (min 1.0) (#(Math/pow %1 (/ 1.0 2.2))) (* 255.0) (int)))

(defn save-image [filename w h pixels]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        pos (for [x (range w) y (range h)] [x y])]
    (doseq [[[x y] p] (map list pos pixels)]
      (.setRGB img x y (.getRGB (Color. (toInt (:x p)) (toInt (:y p)) (toInt (:z p))))))
    (ImageIO/write img "PNG" (File. filename))))

(defn smallpt [w h spp]
  (println "--- smallpt ---")
  (println " usage: lein run [width] [height] [samples]")
  (println (format "  width: %d" w))
  (println (format " height: %d" h))
  (println (format "    spp: %d" spp))
  (let [cam    (ray-init (vec-init 50.0 52.0 295.6) (vec-norm (vec-init 0.0 -0.042612 -1.0)))
        pixels (for [x (range w) y (range (dec h) -1 -1)
                     :let [pixel (reduce vec-add (for [i (range spp)] (vec-mul (trace-path scene cam x y w h spp) (/ 1.0 spp))))]]
                 pixel)]
    (save-image "image.png" w h pixels)))

(defn -main [& args]
  (let [argc (if (nil? args) 0 (count args))
        w    (if (>= argc 1) (Integer/parseInt (nth args 0)) 400)
        h    (if (>= argc 2) (Integer/parseInt (nth args 1)) 300)
        spp  (if (>= argc 3) (Integer/parseInt (nth args 2)) 8)]
        (time (smallpt w h spp))))
