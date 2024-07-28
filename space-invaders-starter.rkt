;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders basic game


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 80)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define EMPTY (square 0 "solid" "white"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2 I3))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListOfInvader

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M1 M2 M3))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - One of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self-reference: (rest lom) is ListOfMissile


(define G0 (make-game LOI0 LOI0 T0))
(define G1 (make-game LOI0 LOI0 T1))
(define G2 (make-game LOI1 LOM1 T1))
(define G3 (make-game LOI2 LOM2 T1))

;; Functions:

;; Game -> Game
;; start the world with G0
;; 
(define (main s)
  (big-bang s                        ; Game
    (on-tick   tock)         ; Game -> Game
    (to-draw   render)       ; Game -> Image
    (stop-when reach-end?)   ; Game -> Boolean
    (on-key    handle-key))) ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state
;; no check-expects due to randomness
;(define (tock s) s);stub
;<took template from Game>

(define (tock s)
  (make-game (next-loi (game-invaders s) (game-missiles s))
             (next-lom (game-missiles s) (game-invaders s))
             (next-tank (game-tank s))))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; produce the next list of invaders
;; no check-expects due to random adition of invader 
;(define (next-loi loi lom) loi);stub
(define (next-loi loi lom)
  (add-invader (tick-invaders loi lom)))

;; ListOfinvader -> ListOfinvader
;; randomly adds an invader to a list of invaders
(check-random (add-invader LOI0) (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) LOI0)) 
(check-random (add-invader LOI2) (if (< (random INVADE-RATE) 1)
                                     (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) LOI2)
                                     LOI2))
;(define (add-invader loi)loi);stub

(define (add-invader loi)
  (cond [(empty? loi) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED))]
        [else
         (if (< (random INVADE-RATE) 1)
             (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
             loi)]))

;; ListOfinvader -> ListOfinvader
;; tick invaders increase x by dx and y by INVADER-Y-SPEED, change direction when hitting borders

(check-expect (tick-invaders LOI0 LOM0) LOI0)
(check-expect (tick-invaders LOI1 LOM0)
              (list (make-invader (+ (invader-dx (first LOI1))(invader-x (first LOI1)))
                                  (+ INVADER-Y-SPEED (invader-y (first LOI1)))
                                  (invader-dx (first LOI1)))))
(check-expect (tick-invaders (list (make-invader 3 0 -3) (make-invader (- WIDTH 3) 50 3)) LOM0)
              (list (make-invader 0 (+ INVADER-Y-SPEED 0) 3) (make-invader WIDTH (+ INVADER-Y-SPEED 50) -3)))

;(define (tick-invaders loi) loi);stub

(define (tick-invaders loi lom)
  (cond [(empty? loi) loi]
        [else
         (if (invader-alive? (first loi) lom)
             (cons (tick-invader (first loi))
                   (tick-invaders (rest loi) lom))
             (rest loi))]))


;; Invider ListOfMissile -> Boolean
;; checks if invider is alive (no missiles in HIT-RANGE)
(check-expect (invader-alive? I1 empty) true)
(check-expect (invader-alive? I1 LOM1) true)
(check-expect (invader-alive? (make-invader 50 (+ 3 HEIGHT) 10) empty) false)
(check-expect (invader-alive? (make-invader 80 60 8)
                              (list (make-missile 20 10)(make-missile 85 57))) false)
;(define (invader-alive? invader loi) true);stub

(define (invader-alive? invader lom)
  (cond [(>= (invader-y invader) HEIGHT) false]
        [(empty? lom) true]
        [else
         (if  (and (< (abs (- (invader-x invader)(missile-x (first lom))))
                      HIT-RANGE)
                   (< (abs (- (invader-y invader)(missile-y (first lom))))
                      HIT-RANGE))
              false
              (invader-alive? invader (rest lom)))]))

;; Invader -> Invader
;; Tick invader, increase x by dx and y by INVADER-Y-SPEED, change direction when hitting borders
(check-expect (tick-invader (make-invader  0 0 -3))
              (make-invader -3 (+ INVADER-Y-SPEED 0) 3))
(check-expect (tick-invader (make-invader WIDTH 80 3))
              (make-invader (+ 3 WIDTH) (+ INVADER-Y-SPEED 80) -3))

;(define (tick-invader invader) invader);stub

(define (tick-invader invader)
  (make-invader
   (+ (invader-dx invader) (invader-x invader))
   (+ INVADER-Y-SPEED (invader-y invader))
   (if (or (and (< (invader-x invader) (/(image-width INVADER) 2)) (< (invader-dx invader) 0))
           (and (> (invader-x invader) (- WIDTH (/(image-width INVADER) 2))) (> (invader-dx invader) 0)))
       (- (invader-dx invader))
       (invader-dx invader))))


;; ListOfMissile ListOfInvader-> ListOfMissile
;; produce the next list of missiles
(check-expect (next-lom LOM0 LOI0) empty)
(check-expect (next-lom LOM1 LOI0) (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))))
;(define (next-lom lom loi) lom);stub
(define (next-lom lom loi)
  (cond [(empty? lom) empty]
        [else
         (if  (missile-alive? (first lom) loi)
              (cons (tick-missile (first lom))
                    (next-lom (rest lom) loi))
              (next-lom (rest lom) loi))]))

;; Missile ListOfInvaders -> Boolean
;; check if a missile is alive (in screen and outside invaders range)
(check-expect (missile-alive? (make-missile 80 0) LOI0) false)
(check-expect (missile-alive? (make-missile 100 200)
                              (list (make-invader 200 300 8))) true)
(check-expect (missile-alive? (make-missile 150 250)
                              (list (make-invader 200 300 8)
                                    (make-invader 148 252 -7))) false)

;(define (missile-alive? missile loi) true);stub
(define (missile-alive? missile loi)
  (cond [(<= (missile-y missile) 0) false]
        [(empty? loi) true]
        [else
         (if  (and (< (abs(- (missile-x missile)(invader-x (first loi))))
                      HIT-RANGE)
                   (< (abs(- (missile-y missile)(invader-y (first loi))))
                      HIT-RANGE))
              false
              (missile-alive? missile (rest loi)))]))

;; Missile -> Missile
;; tick missile
(check-expect (tick-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
;(define (tick-missile missile) missile);stub
(define (tick-missile missile)
  (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED)))

;; Tank -> Tank
;; produce the next tank

(check-expect (next-tank (make-tank 3 -1)) (make-tank 3 -1))
(check-expect (next-tank (make-tank (- WIDTH 3) 1)) (make-tank (- WIDTH 3) 1))
(check-expect (next-tank T0) (make-tank (+ (* (tank-dir T0) TANK-SPEED) (tank-x T0)) (tank-dir T0)))
(check-expect (next-tank T2) (make-tank (+ (* (tank-dir T2) TANK-SPEED) (tank-x T2)) (tank-dir T2)))

;(define (next-tank tank) tank);stub
(define (next-tank tank)
  (if (or (and (< (tank-x tank) (/ (image-width TANK) 2))(= (tank-dir tank) -1))
          (and (> (tank-x tank) (- WIDTH (/ (image-width TANK) 2)))(= (tank-dir tank) 1)))
      tank
      (make-tank (+ (* (tank-dir tank) TANK-SPEED) (tank-x tank)) (tank-dir tank))))

;; Game -> Image
;; Render the game 
(check-expect (render G0)
              (render-loi (game-invaders G0)
                          (render-lom (game-missiles G0)
                                      (render-tank (game-tank G0)
                                                   BACKGROUND))))

;(define (render s) BACKGROUND);stub
(define (render s)
  (render-loi (game-invaders s)
              (render-lom (game-missiles s)
                          (render-tank (game-tank s) BACKGROUND))))

;; ListOfInvader -> Image
;; render invaders
(check-expect (render-loi LOI0 BACKGROUND) BACKGROUND)
(check-expect (render-loi LOI1 BACKGROUND)
              (place-image INVADER (invader-x (first LOI1)) (invader-y (first LOI1))
                           BACKGROUND))

;(define (render-loi loi bgd) EMPTY);stub
(define (render-loi loi bgd)
  (cond [(empty? loi) bgd]
        [else
         (place-image
          INVADER
          (invader-x (first loi))
          (invader-y (first loi))
          (render-loi (rest loi) bgd))]))

;; ListOfMissile -> Image
;; render missiles
(check-expect (render-lom LOM0 BACKGROUND) BACKGROUND)
(check-expect (render-lom LOM1 BACKGROUND)
              (place-image MISSILE (missile-x (first LOM1)) (missile-y (first LOM1))
                           BACKGROUND))

;(define (render-lom lom bgd) BACKGROUND);stub
(define (render-lom lom bgd)
  (cond [(empty? lom) bgd]
        [else
         (place-image
          MISSILE
          (missile-x (first lom))
          (missile-y (first lom))
          (render-lom (rest lom) bgd))]))
;; Tank -> Image
;; render tank
(check-expect (render-tank (make-tank 50 1) BACKGROUND) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
;(define (render-tank tank bgd) bgd);stub
(define (render-tank tank bgd)
  (place-image TANK (tank-x tank) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; Game -> Boolean
;; checks if an invader has reached the end (= invader-y HEIGHT)
(check-expect (reach-end? G0) false)
(check-expect (reach-end? G2) false)
(check-expect (reach-end? (make-game (list (make-invader 0 HEIGHT 15)) empty T1)) true)

;(define (reach-end? s) false);stub
(define (reach-end? s)
  (reach-end-list? (game-invaders s)))

;; ListOfInvader -> Boolean
;; checks if an invader reaches the end
(check-expect (reach-end-list? LOI0) false)
(check-expect (reach-end-list? (list (make-invader 0 0 19))) false)
(check-expect (reach-end-list? (list (make-invader 0 HEIGHT 15) (make-invader 15 0 10))) true)

;(define (reach-end-list? loi) false);stub
(define (reach-end-list? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (reach-end-list? (rest loi)))]))

;; Game KeyEvent -> Game
;; Handle keys (spacebar, left-arrow, right-arrow)

(check-expect (handle-key G0 " ")
              (make-game
               empty
               (list (make-missile (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2)))
               (game-tank G0)))

(check-expect (handle-key G0 "a") G0)

;(define (handle-key s ke) s);stub
(define (handle-key s ke)
  (cond [(key=? ke " ") (make-game
                         (game-invaders s)
                         (cons (make-missile (tank-x (game-tank s)) (- HEIGHT TANK-HEIGHT/2))
                               (game-missiles s))
                         (game-tank s))]
        [(key=? ke "left")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s)) (- (abs (tank-dir (game-tank s))))))]
        [(key=? ke "right")
         (make-game (game-invaders s)
                    (game-missiles s)
                    (make-tank (tank-x (game-tank s)) (abs (tank-dir (game-tank s)))))]
        [else s]))