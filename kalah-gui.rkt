#lang racket
(require racket/gui/base)

(provide topbut
         botbut
         make-gui-move
         frame
         display-info
         print-to-log
         make-frame
         make-first)

(define move-gui 0)

;;функция, возвращающая ход, сделанный через GUI
(define (make-gui-move)
  (set! move-gui 0)
  (send frame show #t)
  move-gui)

;;Функция добавления сообщения в лог
(define (print-to-log lg)
    (new message% [parent log]
                  [label lg]
                  [auto-resize #t]))

;;Функция вывода сообщения в информационное поле
(define (display-info inf)
  (send info set-label inf))

(define frame 'empty)
(define botbut 'empty)
(define topbut 'empty)
(define info 'empty)
(define log 'empty)

;;Функция создания первого диалогового окна
(define (make-first call1 call2 call3)
  (define frame (new dialog% [label "Калах"]
                      [alignment '(center center)]
                   ))
  (define fnt (make-object font% 14 'system))
  (define but-users (new button% [parent frame]
                               [label "Новая игра (люди)"]
                               [vert-margin 8]
                               [horiz-margin 8]
                               [min-width 330]
                               [min-height 50]
                               [font fnt]
                               [callback (lambda (b e) (send frame show #f) (call1 (send slider get-value))) ]
                               ))

  (define but-user-play (new button% [parent frame]
                               [label "Новая игра (человек-минимакс)"]
                               [font fnt]
                               [min-width 330]
                               [min-height 50]
                               [vert-margin 8]
                               [callback (lambda (b e) (send frame show #f) (call2 (send slider get-value)))]
                               ))

  (define but-players (new button% [parent frame]
                               [label "Новая игра (минимаксы)"]
                               [font fnt]
                               [min-width 330]
                               [min-height 50]
                               [vert-margin 8]
                               [callback (lambda (b e) (send frame show #f) (call3 (send slider get-value)))]
                               ))
  (define slider (new slider%	 
                      [label "Размер поля"]	 
                      [parent frame]
                      [font fnt]
                      [vert-margin 8]
                      [horiz-margin 8]
                      [min-value 3]	 
                      [max-value 6]
                      [init-value 6]
                      [style '(horizontal vertical-label)]))
  (send frame show #t))

;;Функция создания игрового окна
(define (make-frame cl)
  (set! frame (new cl [label "Калах"]
                      [width 1000]
                      [height 600]
                   ))
  
  (define panel1 (new horizontal-panel% [parent frame]
                      [style '(border)]
                      [min-height 400]))
  (define kalah1 (new panel% [parent panel1]
                      [style '(border)]
                      [min-width 200]))
  (define board (new vertical-panel% [parent panel1]
                     [style '(border)]
                     [min-width 600]))
  (define kalah2 (new panel% [parent panel1]
                      [style '(border)]
                      [min-width 200]))
  (set! log (new vertical-panel% [parent panel1]
                   [style '(vscroll)]
                   [min-width 250]))

  (define fnt (make-object font% 36 'system))
  (define fnt1 (make-object font% 48 'system))
  (define klht (new message% [parent kalah1]
                    [label "0"]
                    [font fnt1]
                    [auto-resize #t]))
  (define klhb (new message% [parent kalah2]
                    [label "0"]
                    [font fnt1]
                    [auto-resize #t]))

  (define top (new horizontal-panel% [parent board]
                   [style '(border)]
                   [alignment '(center center)]
                   [min-height 200]))

  (define infopanel (new panel% [parent board]
                         [style '(border)]
                         [min-height 40]
                         ))
  (set! info (new message% [parent infopanel]
                    [label ""]
                    [auto-resize #t]))

  (define bottom (new horizontal-panel% [parent board]
                      [style '(border)]
                      [alignment '(center center)]
                      [min-height 200]
                      ))

  (set! topbut (make-vector 6))
  (for ([i '(5 4 3 2 1 0)])
    (vector-set! topbut i (new button% [parent top]
                               [label (number->string i)]
                               [font fnt]
                               [horiz-margin 8]
                               [callback (lambda (b e) (set! move-gui (+ i 1)) (send frame show #f))])))

  (set! topbut (vector-append topbut (vector klht)))


  (set! botbut (build-vector 6 (lambda (x)(new button% [parent bottom]
                                                 [label (number->string x)]
                                                 [font fnt]
                                                 [horiz-margin 8]
                                                 [callback (lambda (b e) (set! move-gui (+ x 1)) (send frame show #f))]))))
  (set! botbut (vector-append botbut (vector klhb))))

