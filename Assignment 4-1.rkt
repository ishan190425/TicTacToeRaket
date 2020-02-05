;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Nick Pelletier, Ishan Rathi, Seth Coellner
(require 2htdp/image)
(require 2htdp/universe)

;A move is one of X-IMG, O-IMG, or BLANK.

;A WorldState is a (make-worldstate (listof (listof move)) Boolean Natural[0, 2]).
;interp
;  board is a representation of the current board state using lists.
;  state is one of the following:
;    "" if the game is not in an end state
;    "draw" if the game has ended in a draw
;    "X" if X has won
;    "O" if O has won
;  AI represents the number of moves that the AI is looking ahead
(define-struct ws (board state AI))

(define SIZE 500)

(define X-IMG (text "X" (/ SIZE 4) "red"))
(define O-IMG (text "O" (/ SIZE 4) "blue"))
(define BLANK (square 0 "solid" "white"))

(define MTS (empty-scene SIZE SIZE))

(define WIDE (rectangle (* SIZE .02) (* SIZE .8) "solid" "black"))

(define TALL (rectangle (* SIZE .8) (* SIZE .02) "solid" "black"))

(define BORDER0 (+ (* SIZE (/ .8 3)) (* SIZE .1)))
(define BORDER1 (+ (* (* SIZE (/ .8 3)) 2) (* SIZE .1)))

(define MTB (place-image TALL (/ SIZE 2) BORDER0
                         (place-image TALL (/ SIZE 2) BORDER1
                                      (place-image WIDE BORDER0 (/ SIZE 2)
                                                   (place-image WIDE BORDER1 (/ SIZE 2) MTS)))))

(define POS0 (+ (* SIZE (/ .8 6)) (* SIZE .1)))
(define POS1 (/ SIZE 2))
(define POS2 (+ (* (* SIZE (/ .8 6)) 5) (* SIZE .1)))
(define POSITIONS (list POS0 POS1 POS2))

(define START (make-ws (list (list BLANK BLANK BLANK)
                             (list BLANK BLANK BLANK)
                             (list BLANK BLANK BLANK)) "" 0))

;---------------------------------------- set/ref ----------------------------------------
;All top-level functions in this secton are top-level because they are used throughout the
;other functions in this program.

;(listof (listof X)) Natural Natural -> X
;Similar to list-ref, but instead finds an element in a 2D list.
(define (list-ref-2d lox x y)
  (list-ref (list-ref lox y) x))
;Test Cases
(check-expect (list-ref-2d (list (list 1 2)
                                 (list 3 4)) 1 0) 2)

;(listof (listof X)) Natural Natural X -> (listof (listof X))
;Similar to list-set, but instead changes an element in a 2D list.
(define (list-set-2d lox x y value)
  (local [(define (list-set lox0 pos value)
            (local [(define (list-set0 lox lstacc posacc)
                      ;lstacc: a list of all of the elements of lox0 before the current position
                      ;posacc: the current 0-based index
                      (cond [(= pos posacc) (append lstacc (list value) (rest lox))]
                            [else (list-set0 (rest lox) (append lstacc (list (first lox))) (+ 1 posacc))]))]
              (list-set0 lox0 empty 0)))]
    (list-set lox y (list-set (list-ref lox y) x value))))
;Test Cases
(check-expect (list-set-2d (list (list 1 2)
                                 (list 3 4)) 0 1 5) (list (list 1 2)
                                                          (list 5 4)))

;--------------------------------------- outcome -----------------------------------------

;(listof (listof move)) Image -> Boolean
;Tests to see if player has won based on the given board
;This function was defined at top-level because it requires extensive testing
(define (win? board player)
  (local [(define (test-vertical board player)
            (local [(define (vertical x y)
                      ;x: current x-position
                      ;y: current y-position
                      (cond [(= y (length board)) true]
                            [(not (equal? player (list-ref-2d board x y))) false]
                            [else (vertical x (+ 1 y))]))]
              (or (vertical 0 0) (vertical 1 0) (vertical 2 0))))
          (define (test-horizontal board player)
            (local [(define (horizontal x y)
                      ;x: current x-position
                      ;y: current y-position
                      (cond [(= x (length (list-ref board y))) true]
                            [(not (equal? player (list-ref-2d board x y))) false]
                            [else (horizontal (+ 1 x) y)]))]
              (or (horizontal 0 0) (horizontal 0 1) (horizontal 0 2))))
          (define (test-diagonal board player)
            (local [(define (diag1 x y)
                      ;x: current x-position
                      ;y: current y-position
                      (cond [(= y (length board)) true]
                            [(not (equal? player (list-ref-2d board x y))) false]
                            [else (diag1 (+ 1 x) (+ 1 y))]))
                    (define (diag2 x y)
                      ;x: current x-position
                      ;y: current y-position
                      (cond [(= x -1) true]
                            [(not (equal? player (list-ref-2d board x y))) false]
                            [else (diag2 (- x 1) (+ 1 y))]))]
              (or (diag1 0 0)
                  (diag2 2 0))))]
    (or (test-vertical board player)
        (test-horizontal board player)
        (test-diagonal board player))))
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK X-IMG BLANK)
                          (list BLANK BLANK O-IMG)) O-IMG) false) ;no winner (test O-IMG)
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK X-IMG BLANK)
                          (list BLANK BLANK O-IMG)) X-IMG) false) ;no winner (test X-IMG)
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK X-IMG BLANK)
                          (list O-IMG O-IMG O-IMG)) O-IMG) true) ;O-IMG horizontal
(check-expect (win? (list (list X-IMG X-IMG X-IMG)
                          (list BLANK O-IMG BLANK)
                          (list BLANK BLANK O-IMG)) X-IMG) true) ;X-IMG horizontal
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK X-IMG BLANK)
                          (list O-IMG O-IMG O-IMG)) X-IMG) false) ;O-IMG horizontal, but testing X-IMG
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK X-IMG O-IMG)
                          (list O-IMG BLANK O-IMG)) O-IMG) true) ;O-IMG vertical
(check-expect (win? (list (list X-IMG X-IMG O-IMG)
                          (list BLANK X-IMG BLANK)
                          (list O-IMG X-IMG O-IMG)) X-IMG) true) ;X-IMG vertical
(check-expect (win? (list (list X-IMG X-IMG O-IMG)
                          (list BLANK X-IMG BLANK)
                          (list O-IMG X-IMG O-IMG)) O-IMG) false) ;X-IMG vertical, but testing O-IMG
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK X-IMG BLANK)
                          (list O-IMG BLANK X-IMG)) X-IMG) true) ;X-IMG diag1
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK O-IMG X-IMG)
                          (list O-IMG BLANK X-IMG)) O-IMG) true) ;O-IMG diag2
(check-expect (win? (list (list X-IMG BLANK O-IMG)
                          (list BLANK O-IMG X-IMG)
                          (list O-IMG BLANK X-IMG)) X-IMG) false) ;O-IMG diag2, but testing X-IMG

;(listof (listof move)) -> String
;Tests to see if the game has ended and returns a String representing the current state:
;  "X" if X has won
;  "O" if O has won
;  "draw" if the game has eded in a draw (i.e. the board is filled and neither X nor O has won)
;  "" if the game is not over
;This is defined at top level because there are multiple functions that need to see if the game has ended
(define (end? board)
  (local [(define (all-not-equal-2d? lox val)
            (local [(define (all-not-equal? lox val)
                      (cond [(empty? lox) true]
                            [(equal? (first lox) val) false]
                            [else (all-not-equal? (rest lox) val)]))]
              (cond [(empty? lox) true]
                    [(not (all-not-equal? (first lox) val)) false]
                    [else (all-not-equal-2d? (rest lox) val)])))]
    (cond
      [(win? board X-IMG) "X"]
      [(win? board O-IMG) "O"]
      [(all-not-equal-2d? board BLANK) "draw"]
      [else ""])))
;Test Cases
(check-expect (end? (list (list X-IMG X-IMG O-IMG)
                          (list O-IMG O-IMG X-IMG)
                          (list X-IMG O-IMG X-IMG))) "draw")
(check-expect (end? (list (list X-IMG O-IMG O-IMG)
                          (list O-IMG X-IMG X-IMG)
                          (list O-IMG X-IMG X-IMG))) "X")
(check-expect (end? (list (list X-IMG X-IMG O-IMG)
                          (list O-IMG O-IMG X-IMG)
                          (list O-IMG X-IMG X-IMG))) "O")
(check-expect (end? (ws-board START)) "")

;----------------------------------------- draw ------------------------------------------

;WorldState Natural[0,2] Natural[0,2] Image -> Image
;Places the image at position (x,y) in ws into the correct position on the board in the given scene.
;This is defined at top-level because it makes it and draw easier to test.
(define (place-move ws x y scene)
  (place-image (list-ref-2d (ws-board ws) x y)
               (list-ref POSITIONS x)
               (list-ref POSITIONS y)
               scene))
(check-expect (place-move (make-ws (list (list X-IMG BLANK BLANK)
                                         (list BLANK BLANK BLANK)
                                         (list BLANK BLANK BLANK)) "" 0) 0 0 MTB) (place-image X-IMG
                                                                                               (list-ref POSITIONS 0)
                                                                                               (list-ref POSITIONS 0)
                                                                                               MTB))

;WorldState -> Image
;Renders the tic-tac-toe board based on (ws-board ws). Then, checks (ws-state ws) to see if there is
;a win or a tie.
;This is defined at top-level because it makes it easier to test
(define (draw ws)
  (local [(define (place-moves ws scene x y)
            (cond [(and (= x 2) (= y 2))
                   (place-move ws x y scene)]
                  [(= x 2)
                   (place-moves ws (place-move ws x y scene) 0 (+ y 1))]
                  [else
                   (place-moves ws (place-move ws x y scene) (+ x 1) y)]))]
    (cond [(string=? (ws-state ws) "draw")
           (place-image (text "DRAW" (/ SIZE 4) "green")
                        (/ SIZE 2)
                        (/ SIZE 2)
                        (place-moves ws MTB 0 0))]
          [(string=? (ws-state ws) "")
           (place-moves ws MTB 0 0)]
          [else
           (place-image (text (string-append (ws-state ws) " WINS") (/ SIZE 4) "green")
                        (/ SIZE 2)
                        (/ SIZE 2)
                        (place-moves ws MTB 0 0))])))
;Test Cases
(check-expect (draw START) MTB)
(check-expect (draw (make-ws (list (list BLANK X-IMG BLANK)
                                   (list BLANK BLANK BLANK)
                                   (list BLANK BLANK BLANK)) "" 0)) (place-move (make-ws (list (list BLANK X-IMG BLANK)
                                                                                               (list BLANK BLANK BLANK)
                                                                                               (list BLANK BLANK BLANK)) "" 0) 1 0 MTB))
(check-expect (draw (make-ws (list (list BLANK BLANK BLANK)
                                   (list BLANK BLANK BLANK)
                                   (list BLANK BLANK BLANK)) "draw" 0)) (place-image (text "DRAW" (/ SIZE 4) "green")
                                                                                     (/ SIZE 2)
                                                                                     (/ SIZE 2)
                                                                                     MTB))
(check-expect (draw (make-ws (list (list BLANK BLANK BLANK)
                                   (list BLANK BLANK BLANK)
                                   (list BLANK BLANK BLANK)) "X" 0)) (place-image (text "X WINS" (/ SIZE 4) "green")
                                                                                  (/ SIZE 2)
                                                                                  (/ SIZE 2)
                                                                                  MTB))

;----------------------------------------- mouse -----------------------------------------

;Number -> Natural
;Converts a mouse coordinate into an index to be used by add-move. Returns -1 if the coordinate is outside the board.
;This is defined at top-level because it is used by multiple other functions
(define (test-pos n)
  (cond [(and (> n (* SIZE .1)) (<= n BORDER0)) 0]
        [(and (> n BORDER0) (<= n BORDER1)) 1]
        [(and (> n BORDER1) (<= n (* SIZE .9))) 2]
        [else -1]))
(check-expect (test-pos (* SIZE .2)) 0)
(check-expect (test-pos (/ SIZE 2)) 1)
(check-expect (test-pos (* SIZE .8)) 2)
(check-expect (test-pos (- SIZE 1)) -1)

;WorldState Natural Natural -> WorldState
;Makes the player's move based on the mouse coordinates x and y, then calls AI-make-move if the game has not ended
;This is at top-level because it makes it easier to test
(define (do-round ws x y)
  (local [(define (add-move board x y img)
            (list-set-2d board (test-pos x) (test-pos y) img))
          (define (AI-make-move ws)
            (local [(define lop (find-open (ws-board ws)))]
              (cond [(= (ws-AI ws) 0)
                     (local [(define p (list-ref lop (random (length lop))))]
                       (list-set-2d (ws-board ws) (pos-x p) (pos-y p) O-IMG))]  ;I can't test this with check-expect becasue it makes a random move
                    [(= (ws-AI ws) 1) (check-1-ahead (ws-board ws) lop)]
                    [(= (ws-AI ws) 2) (check-2-ahead (ws-board ws) lop)])))]
    (cond [(or (< (test-pos x) 0) (< (test-pos y) 0)) ws]
          [(not (equal? (list-ref-2d (ws-board ws) (test-pos x) (test-pos y)) BLANK)) ws]
          [else
           (local [(define playermove (add-move (ws-board ws) x y X-IMG))]
             (cond [(not (string=? (end? playermove) "")) (make-ws playermove (end? playermove) (ws-AI ws))]
                   [else
                    (local [(define AImove (AI-make-move (make-ws playermove (ws-state ws) (ws-AI ws))))]
                      (make-ws AImove (end? AImove) (ws-AI ws)))]))])))
;Test Cases
(define WS1 (make-ws (list (list BLANK O-IMG O-IMG)
                           (list BLANK BLANK X-IMG)
                           (list BLANK X-IMG X-IMG)) "" 2))
(define WS4 (make-ws (list (list BLANK O-IMG O-IMG)
                           (list BLANK O-IMG X-IMG)
                           (list BLANK X-IMG X-IMG)) "" 1))
(define WS5 (make-ws (list (list BLANK O-IMG O-IMG)
                           (list BLANK BLANK X-IMG)
                           (list BLANK X-IMG X-IMG)) "" 1))
(define WS6 (make-ws (list (list BLANK O-IMG O-IMG)
                           (list X-IMG BLANK X-IMG)
                           (list O-IMG X-IMG X-IMG)) "X" 1))
(check-expect (do-round WS1 (/ SIZE 2) (/ SIZE 2)) (make-ws (list (list O-IMG O-IMG O-IMG)
                                                                  (list BLANK X-IMG X-IMG)
                                                                  (list BLANK X-IMG X-IMG)) "O" 2))
(check-expect (do-round WS4 (/ SIZE 2) (/ SIZE 2)) (make-ws (list (list BLANK O-IMG O-IMG)
                                                                  (list BLANK O-IMG X-IMG)
                                                                  (list BLANK X-IMG X-IMG)) "" 1))
(check-expect (do-round WS5 (/ SIZE 2) (/ SIZE 2)) (make-ws (list (list O-IMG O-IMG O-IMG)
                                                                  (list BLANK X-IMG X-IMG)
                                                                  (list BLANK X-IMG X-IMG)) "O" 1))
(check-expect (do-round WS6 (/ SIZE 2) (/ SIZE 2)) (make-ws (list (list BLANK O-IMG O-IMG)
                                                                  (list X-IMG X-IMG X-IMG)
                                                                  (list O-IMG X-IMG X-IMG)) "X" 1))

;WorldState Number Number MouseEvent -> WorldState
;Adds an X in the location where the mouse is clicked, according to add-move.
;This is top level because it makes it easier to test.
(define (mouse ws x y me)
  (cond [(not (string=? (ws-state ws) "")) ws]
        [(mouse=? me "button-down")
         (do-round ws x y)]
        [else ws]))
(define WS2 (make-ws empty "O" 1))
(check-expect (mouse WS2 30 30 "button-down") WS2)
(check-expect (mouse START 40 50 "button-down") (do-round START 40 50))
(check-expect (mouse START 30 40 "button-up") START)

;------------------------------------------ AI -------------------------------------------

;A pos is a (make-pos Integer Integer)
;interp
;  x is the x-position
;  y is the y-position
(define-struct pos (x y))

;(listof (listof move)) -> (listof pos)
;Returns a list of pos that represents all of the positions in board0 where the element is equal to BLANK
;This function is top-level because it makes it and check-1-ahead easier to test.
(define (find-open board0)
  (local [(define (find-open board y)
            ;y: the current y-position
            (cond [(empty? board) empty]
                  [else
                   (append (find-open-helper (first board) empty 0 y) (find-open (rest board) (+ 1 y)))]))
          (define (find-open-helper lom lop x y)
            ;lop: the positions of all of the previous BLANK elements
            ;x: the current x-position
            ;y: the current y-position
            (cond [(empty? lom) lop]
                  [(equal? (first lom) BLANK) (find-open-helper (rest lom)
                                                                (cons (make-pos x y) lop)
                                                                (+ 1 x) y)]
                  [else (find-open-helper (rest lom)
                                          lop
                                          (+ 1 x) y)]))]
    (find-open board0 0)))
;Test Cases:
(check-expect (find-open (list (list X-IMG O-IMG BLANK)
                               (list X-IMG O-IMG X-IMG)
                               (list O-IMG BLANK X-IMG))) (list (make-pos 2 0) (make-pos 1 2)))

;(listof (listof move)) (listof pos) -> (listof (listof move))
;Checks to see if there is a move that O can make that will result in a win for O
;This function is top-level because it is used in multiple other functions
(define (check-1-ahead board lop)
  (local [(define (check-1-ahead-help board lop length lop0)
            (cond [(empty? lop)
                   (local [(define p (list-ref lop0 (random length)))]
                     (list-set-2d board (pos-x p) (pos-y p) O-IMG))]    ;I can't test this piece of code with check-expect because it makes a random move
                  [(string=? (end? (list-set-2d board
                                                (pos-x (first lop))
                                                (pos-y (first lop))
                                                O-IMG)) "O")
                   (list-set-2d board
                                (pos-x (first lop))
                                (pos-y (first lop))
                                O-IMG)]
                  [else (check-1-ahead-help board (rest lop) length lop0)]))]
    (check-1-ahead-help board lop (length lop) lop)))
;Test Cases
(define BOARD1 (list (list X-IMG BLANK X-IMG)
                     (list BLANK O-IMG O-IMG)
                     (list BLANK X-IMG BLANK)))
(check-expect (check-1-ahead BOARD1 (find-open BOARD1)) (list (list X-IMG BLANK X-IMG)
                                                              (list O-IMG O-IMG O-IMG)
                                                              (list BLANK X-IMG BLANK)))
;(listof (listof move)) (listof pos) -> (listof (listof move))
;Checks to see if there is a move that O can make that will result in a win for O using check-1-ahead. If
;there is no such move, it will then check to see if there is a move that will prevent X from winning on
;its next turn.
;This is top-level because it needs to be tested, since it is very complex
(define (check-2-ahead board lop)
  (cond [(string=? (end? (check-1-ahead board lop)) "O") (check-1-ahead board lop)]
        [else
         (local [(define (check-2-ahead-help board lop length lop0)
                   (cond [(empty? lop)
                          (local [(define p (list-ref lop0 (random length)))]
                            (list-set-2d board (pos-x p) (pos-y p) O-IMG))]    ;I can't test this piece of code with check-expect because it makes a random move
                         [(string=? (end? (list-set-2d board
                                                       (pos-x (first lop))
                                                       (pos-y (first lop))
                                                       X-IMG)) "X")
                          (list-set-2d board
                                       (pos-x (first lop))
                                       (pos-y (first lop))
                                       O-IMG)]
                         [else (check-2-ahead-help board (rest lop) length lop0)]))]
           (check-2-ahead-help board lop (length lop) lop))]))
;Test Cases
(define BOARD2 (list (list BLANK BLANK O-IMG)
                     (list X-IMG BLANK X-IMG)
                     (list BLANK X-IMG BLANK)))
(check-expect (check-2-ahead BOARD1 (find-open BOARD1)) (list (list X-IMG BLANK X-IMG)
                                                              (list O-IMG O-IMG O-IMG)
                                                              (list BLANK X-IMG BLANK)))
(check-expect (check-2-ahead BOARD2 (find-open BOARD2)) (list (list BLANK BLANK O-IMG)
                                                              (list X-IMG O-IMG X-IMG)
                                                              (list BLANK X-IMG BLANK)))
   
;----------------------------------------- key -------------------------------------------

;WorldState KeyEvent -> WorldState
;Decides AI level based on the number pressed.
;This is at top-level becasue it makes it easier to test.
(define (key ws ke)
  (cond [(key=? ke "0") (make-ws (ws-board ws) (ws-state ws) 0)]
        [(key=? ke "1") (make-ws (ws-board ws) (ws-state ws) 1)]
        [(key=? ke "2") (make-ws (ws-board ws) (ws-state ws) 2)]))
;Test Cases
(check-expect (key START "0") (make-ws (ws-board START) (ws-state START) 0))
(check-expect (key START "1") (make-ws (ws-board START) (ws-state START) 1))
(check-expect (key START "2") (make-ws (ws-board START) (ws-state START) 2))

;----------------------------------------- main ------------------------------------------

(define (main ws)
  (big-bang ws
    (to-draw draw)
    (on-mouse mouse)
    (on-key key)))

(main START)