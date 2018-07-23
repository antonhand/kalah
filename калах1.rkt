#lang racket

(require lazy/force)
(require "kalah-gui.rkt" racket/gui/base)

(define game%
  (class object%
    (super-new)
    ;; виртуальные методы для задания правил игры
    (init-field
     my-win? ; State -> Bool
     my-lose? ; State -> Bool
     draw-game? ; State -> Bool
     my-move ; State Move -> (State Last-field)
     opponent-move ; State Move -> State
     possible-moves ; State -> (list Move)
     show-state ; State -> Any
    
     )
    ;; optimal-move :: State -> Move -- выбор оптимального хода
    (define/public ((optimal-move bside look-ahead) S)
      (argmax (lambda (m) (minimax (game-tree S m look-ahead)))
              (shuffle (possible-moves bside S))))
    ;;game-tree :: State -> (Move -> (Tree of Real)) построение дерева
    (define/public (game-tree St m look-ahead) (void))

    
    (define/public (make-move S move)
      (cond
        (((! my-win?) S) (values '() S 'win))
        (((! my-lose?) S) (values '() S 'lose))
        ((draw-game? S) (values '() S 'draw))
        (else (let ((m* (move S)))
                (define-values (S* lf) (my-move S m*))
                (cond
                  (((! my-win?) S*) (values m* S* 'win))
                  (((! my-lose?) S*) (values m* S* 'lose))
                  ((draw-game? S*) (values m* S* 'draw))
                  ((= lf 6) (values m* S* 'another)) ; если последнее зерно было положено в калах, даётся дополнительный ход
                  ((= lf -1) (values m* S* 'bad))
                  (else (values m* S* 'next)))))))))

;; Представление ситуации на доске.
(struct board (top bottom))
;; геттеры полей структуры
(define btop board-top)
(define bbot board-bottom)
;; функция, возвращающая противоположный геттер
(define (bopposite bside)
  (cond ((eq? bside btop) bbot)
        ((eq? bside bbot) btop)))

;; в начале на игровой доске во всех лунках, кроме калахов по 6 зёрен, в калахах зёрен нет
(define (board-n n) (board (vector n n n n n n 0) (vector n n n n n n 0)))

;; игра заканчивается, когда у одного из игроков во всех лунках, кроме калаха не остаётся зёрен
(define (is-final? b)
  (or (= (foldl + 0 (vector->list (btop b))) (vector-ref (btop b) 6)) 
      (= (foldl + 0 (vector->list (bbot b))) (vector-ref (bbot b) 6))))

;; проверка, есть ли выигрыш на доске
;; игрок выигрывет, если в его калахе оказывается более половины зёрен,
;; либо, когда  игра завершена и на его стороне больше зёрен, чем на стороне противника (включая все лунки и калахи)
(define ((wins? bside bsidesize) b)
  (or (> (vector-ref (bside b) 6) bsidesize)
      (and (is-final? b)
           (> (foldl + 0 (vector->list (bside b))) (foldl + 0 (vector->list((bopposite bside) b)))))))

;; проверка, есть ли ничья на доске
;; ничья возникает, когда игра закончена и на обоих сторонах одинаковое количество зёрен
(define (draw? b)
  (and (is-final? b)
       (= (foldl + 0 (vector->list (btop b))) (foldl + 0 (vector->list(bbot b))))))

;; функция, вычисляющая лунки, в которых есть камни
(define (nonfree-pits bside b)
  (let ((res '()))
    (for ([i '(1 2 3 4 5 6)])
      (when (> (vector-ref (bside b) (- i 1)) 0)
        (set! res (cons i res))))
    res))

;; функция для осуществления ходов игроков
(define ((move bside) brd m . silence)
  (display-info "")
  (let ((b (board (vector-copy (btop brd))  (vector-copy (bbot brd)))))
    (if (= (vector-ref (bside b) (- m 1)) 0)
        (begin
          (displayln "Нельзя взять оттуда, где ничего нет.")
          (values b -1))        
        (let ((num-init (vector-ref (bside b) (- m 1))))
          ;;обход поля и оставление камней в соответствующих лунках
          (define (loop b m num side)
            (let ((new-val (+ 1 (vector-ref (side b) m))))
              (vector-set! (side b) m new-val)
              (if (<= num 1) (values b m side)       ; останавливаемся, когда положим последнее зерно, возвращаем доску, номер ячейки и сторону, в которой остановились
                  (let* ((last (if (eq? side bside) 6 5)) ;в свой калах кладём камень, в чужой - нет
                         (new-m (if (= m last) 0 (+ 1 m)))
                         (new-side (if (= new-m 0) (bopposite side) side))) 
                    (loop b new-m (- num 1) new-side))))) ;обходим поле против часовой стрелки циклично
  
          (vector-set! (bside b) (- m 1) 0) ; опусташаем выбранную лунку
          (define-values (bs last-field side)(loop b m num-init bside))
          ; если последнее зерно было положено в пустую лунку на своей стороне, то это зерно и все зёрна из противоположной лунки соперника отправляются в свой калах
          (if (and (< last-field 6) (eq? bside side) (= (vector-ref (side bs) last-field) 1))
              (begin (vector-set! (side bs) 6 (+ (vector-ref (side bs) 6) (vector-ref ((bopposite side) bs) (- 5 last-field)) 1))
                     (vector-set! (side bs) last-field 0)
                     (vector-set! ((bopposite side) bs) (- 5 last-field) 0)
                     (if (null? silence)
                         (display-info "Последний камень в своей пустой лунке — камень из этой лунки и чужой лунки напротив в ваш калах!")
                         (void))
                     (values bs last-field))
              (values bs last-field)))))) ; возвращаем поле и номер ячейки, в которой остановились


;; вывод игровой доски в текстовом виде
(define ((show-board mode) b)
  (if (or (eq? mode 'top) (eq? mode 'full))
      (begin (display "     ")
             (for ([i '(5 4 3 2 1 0)])
               (if (or (eq? mode 'full) (> (vector-ref (btop b) i) 0))
                   (printf "  ~a  " (+ 1 i))
                   (printf "  -  ")))
             (display "    \n▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄\n█    "))
      (display "▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄\n█    "))
  (for ([i '(5 4 3 2 1 0)])
    (let ((n (vector-ref (btop b) i)))
      (if (< n 10)
          (printf "█ 0~a " n)
          (printf "█ ~a " n))))
  (display "█    █\n")
  (let ((k1 (vector-ref (btop b) 6)))
    (if (< k1 10)
        (printf "█ 0~a" k1)
        (printf "█ ~a" k1)))
  (display " ███████████████████████████████ ")
  (let ((k2 (vector-ref (bbot b) 6)))
    (if (< k2 10)
        (printf "0~a █\n█    " k2)
        (printf "~a █\n█    " k2)))
  (for ([i '(0 1 2 3 4 5)])
    (let ((n (vector-ref (bbot b) i)))
      (if (< n 10)
          (printf "█ 0~a " n)
          (printf "█ ~a " n))))
  (display "█    █\n")
  (if (or (eq? mode 'bottom) (eq? mode 'full))
      (begin (displayln "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀")
             (display "     ")
             (for ([i '(0 1 2 3 4 5)])
               (if (or (eq? mode 'full) (> (vector-ref (bbot b) i) 0))
                   (printf "  ~a  " (+ 1 i))
                   (printf "  -  ")))
             (displayln ""))
      (displayln "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀")))

;; вывод игровой доски в GUI
(define ((upd-gui mode) b)
  (vector-map (lambda (x y)
                (send x set-label (number->string y))
                (if (and (= y 0) (is-a? x button%))
                    (send x enable #f)
                    (send x enable #t)))
              topbut (btop b))
  (vector-map (lambda (x y)
                (send x set-label (number->string y))
                (if (and (= y 0) (is-a? x button%))
                    (send x enable #f)
                    (send x enable #t)))
              botbut (bbot b)) 
  (when (or (eq? mode 'top) (eq? mode 'full))
    (for ([i '(0 1 2 3 4 5)])
      (send (vector-ref botbut i) enable #f)))
  (when (or (eq? mode 'bottom) (eq? mode 'full))
    (for ([i '(0 1 2 3 4 5)])
      (send (vector-ref topbut i) enable #f)))
  (when (eq? mode 'full)
    (send frame show #t)))

;; Описание класса-игры калах
(define kalah%
  (class game%
    (super-new
     [draw-game? draw?]
     [my-win? (delay (wins? bside bsidesize))]
     [my-lose? (delay (wins? (bopposite bside) bsidesize))]
     [possible-moves nonfree-pits]
     )
    (init-field bside
                [bsidesize 36])
    (inherit-field  my-win? 
                    my-lose? 
                    draw-game? 
                    my-move
                    opponent-move
                    possible-moves)
    (define/override (game-tree St m look-ahead)
      (define (opposite-move move)
        (if (eq? move my-move) opponent-move my-move))
      
      (define (new-ply move i s mv)
        ; т.к. по правилам игры не всегда ходы чередуются, необходимо запоминать, чей в данной вершине был ход
        (let ((min-max (if (eq? move my-move) 'max 'min)) 
              (side (if (eq? move my-move) bside (bopposite bside))))
          (define-values (S* lf) (move s mv 'silence))
          (cond (((! my-win?) S*) (cons min-max  +inf.0)) ; в выигрышной позиции
                (((! my-lose?) S*) (cons min-max -inf.0)) ; в проигрышной
                ((draw-game? S*) (cons min-max 0)) ; при ничье
                ((>= i look-ahead) (cons min-max (f-h bside S*))) ; если исчерпана глубина
                ((= lf 6) (cons min-max (map (lambda (x) (new-ply move (+ 1 i) S* x))
                                             (possible-moves side S*))))
                (else (cons min-max (map (lambda (x) (new-ply (opposite-move move) (+ 1 i) S* x))
                                         (possible-moves (bopposite side) S*)))))))
      (new-ply my-move 1 St m))))
     
;; Реализация класса игрока
(define (interactive-player game)
  (class game
    (super-new)
    (inherit-field show-state
                   bside)
    (inherit make-move
             optimal-move)
    (init-field name
                [look-ahead 4]
                [opponent 'undefined]
                [move-method (optimal-move bside look-ahead)])
    (define/public (your-turn S)
      (show-state S)
      (define-values (m S* status) (make-move S move-method))
      (print-to-log (format "~a сделал ход ~a" name m))
      (case status
        ['stop (display-info "Игра была прервана.")]
        ['win  (display-info (format"~a победил!" name))
               ((upd-gui 'full) S*)]
        ['lose (display-info (format "~a победил!" (get-field name opponent)))
               ((upd-gui 'full) S*)]
        ['draw (display-info "Ничья!")
               ((upd-gui 'full) S*)]
        ['another (display-info "Последний камень в калахе — дополнительный ход!")
                  (print-to-log "Дополнительный ход")
                  (send this your-turn S*)]
        ['bad (send this your-turn S*)]
        [else (send opponent your-turn S*)]))))

(define-syntax-rule
  (define-partners game (A #:side A-side #:move A-move #:show A-show)
    (B #:side B-side #:move B-move #:show B-show))
  (begin
    (define A (class game
                (super-new
                 [bside A-side]
                 [my-move A-move]
                 [opponent-move B-move]
                 [show-state A-show])))
    (define B (class game
                (super-new
                 [bside B-side]
                 [my-move B-move]
                 [opponent-move A-move]
                 [show-state B-show])))))

;; заведение партнеров для калаха
(define-partners kalah%
  (top% #:side btop #:move (move btop) #:show (upd-gui 'top))
  (bottom% #:side bbot #:move (move bbot)  #:show (upd-gui 'bottom)))

;; функция, инициализирующая игру
(define (start-game p1 p2 initial-state)
  (set-field! opponent p1 p2)1
  (set-field! opponent p2 p1)
  (let ((bsidesize (/ (+ (foldl + 0 (vector->list (btop initial-state))) (foldl + 0 (vector->list(bbot initial-state)))) 2)))
    (set-field! bsidesize p1 bsidesize)
    (set-field! bsidesize p2 bsidesize))
  (send p1 your-turn initial-state))

(define (input-move m)
  (case m
    ('q (exit))
    ('1 m) ('2 m) ('3 m)
    ('4 m) ('5 m) ('6 m)
    (else (exit))))
(define user-A (new (interactive-player top%)
                    [name "Верхний пользователь"]
                    [move-method (lambda (b) (input-move (make-gui-move)))]))
(define user-B (new (interactive-player bottom%)
                    [name "Нижний пользователь"]
                    [move-method (lambda (b) (input-move (make-gui-move)))]))

;; функция эвристической оценки позиции
;; из количества камней на стороне игрока вычитается количество камней на стороне противника
(define (f-h bside s)
  (- (foldl + 0 (vector->list (bside s))) (foldl + 0 (vector->list((bopposite bside) s)))))

(define (minimax tree)
  (define (minimax-h node alpha beta)
    (define (next-max x v)
      (if (or (null? x) (<= beta v)) v
          (next-max (cdr x)
                    (max v (minimax-h (car x) v beta))))
      )
    (define (next-min x v)
      (if (or (null? x) (<= v alpha)) v
          (next-min (cdr x)
                    (min v (minimax-h (car x) alpha v))))
      )
    (let ((min-or-max (car node)) ;минимум или масимум определяется исходя из метки в данном узле
          (node* (cdr node)))
      (cond
        ((number? node*) node*)
        ((null? node*) 0.0)
        ((eq? min-or-max 'max) (next-max node* alpha))
        (else (next-min node* beta))) ))
  (!(minimax-h tree -inf.0 +inf.0)) )

(define player-A (new (force (interactive-player top%))
                      [name "Верхний минимакс"]
                      [look-ahead 4]))
(define player-B (new (force (interactive-player bottom%))
                      [name "Нижний минимакс"]
                      [look-ahead 4]))

;; создаём первое диалоговое окно, инициалзируя при этом реакции на нажатия кнопок
(make-first (lambda (size)
              (make-frame dialog%)
              (start-game user-A user-B (board-n size)))

            (lambda (size)
              (make-frame dialog%)
              (start-game player-A user-B (board-n size)))

            (lambda (size)
              (make-frame frame%)
              (send frame show #t)
              (start-game player-A player-B (board-n size))))

