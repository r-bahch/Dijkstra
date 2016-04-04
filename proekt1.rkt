;1) BFS

;Помощна за noviVarhove. Приема списък с наследниците на текущия връх - във вид ((C 7) (E 5) (W 3)), опашка от пътища, и път от корена до текущия връх.
;Връща опашката, разширена с пътищата от корена до наследниците на върха
(define (extendQueue! L Q path)
  (if (null? L) Q
      (extendQueue! (cdr L) (insert-queue! Q (cons (car (car L)) path)) path)))
 
;Тази функция приема path(път в графа във формат '(D B A) - A e корена D e последния), vertex (връх на който се търсят наследници, т.е. car na path),
;net (графа, с който работим), Q(опашка от пътища) и връща опашката Q с добавени пътищата от корена до наследниците на vertex
(define (noviVarhove path vertex net Q)
  (let ((L (cdr (assoc vertex net))))
    (extendQueue! L Q path)))

;помощна функция - дефинира опашка, вкарва вътре списък с елемент началния връх и извиква алгоритмичната функция
(define (bfs-help start end G)
  (define Q (make-queue))
  (insert-queue! Q (list start))
  (bfs end Q G))
  
;алгоритмична функция
;извиква се с краен връх end, опашка от пътища Q, Която първоначално съдържа само (А), и граф G
(define (bfs end Q G)
  (if (empty-queue? Q) '()                                ;ако опашката е празна, връщаме празен списък. Няма път между 2та върха                                               
      (let ((path (front Q)))                             ;изключваме път от опашката с пътища
        (let ((node (car path)))                          ;текущ връх  
          (if (equal? node end)                           ;ако текущия връх е крайния
              (reverse path)                              ;то това е търсеният път
              (begin (delete-queue! Q)                    ;иначе изтриваме пътя от опашката
                     (bfs end                          
                               (noviVarhove path node G Q);и извикваме рекурсивнофункцията със видоизменената опашка
                               G)
                     ))))))

;Допълнителна функция, понеже не видях навреме, че трябва да се изведе и дължината на пътя
;Приема списък L, представляващ път в граф, и граф G, и връща теглото на пътя
(define (path-length L G)
  (define (iter-path L counter)
    (if (null? (cdr L)) counter
        (iter-path (cdr L) (+ counter (cadr (assoc (cadr L) (cdr (assoc (car L) G))))))))
  (iter-path L 0))

;Дефиниция на опашка - от учебника
;1. Помощни функции
(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
;2.Основни функции
;проверка за празнота
(define (empty-queue? queue)
  (null? (front-ptr queue)))
;създаване на празна опашка
(define (make-queue)
  (cons '() '()))
;достъп до началото на опашката
(define (front queue)
  (if (empty-queue? queue)
      (display "error: front is called for an empty queue")
      (car (front-ptr queue))))
;добавяне на елемент на края на опашката
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (begin
          (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))
;изтриване на елемент от началото на опашката
(define (delete-queue! queue)
  (if (empty-queue? queue)
      (display "error: deleting an element from empty queue")
      (begin 
        (set-front-ptr! queue (cdr (front-ptr queue)))
        queue)))





;2) Dijkstra
;функция filter - взета от учебника
(define (filter pred? l)
  (cond ((null? l) '())
        ((pred? (car l)) (cons (car l)
                               (filter pred? (cdr l))))
        (else (filter pred? (cdr l)))))

;Инициализация на асоциативен списък, който ще съдържа родителя в МПД на всеки възел на графа
;Приема аргументи: start - началния връх на графа, и G - самия граф
;Връща списък, в който началния връх е асоцииран със себе си, а останалите - с +безкр.
(define (init-previous start G)
   (let ((L (map (lambda (n) (cons (car n) +inf.0)) G))) ;създава се списъка
      (set-cdr! (assoc start L) start)                   ;променяме бащата на началния връх
      L))                                                ;и връщаме получения списък

;Инициализация нa цената на върховете - приема за аргументи граф G и стартов връх start и връща асоциативен списък от всички върхове 
;в който всеки връх е асоцииран с +безкр. а началния - с 0
(define (init-dist start G)
  (let ((L (map (lambda (n) (cons (car n) +inf.0)) G)))  ;създава се списъка
      (set-cdr! (assoc start L) 0)                       ;променяме цената на началния
      L))                                                ;и връщаме получения списък

;Достъп до стойност в асоциативен списък - по връх А и списък от цени или родители dist връща съответно цената или родителя на върха
(define (get-value A dist)
 (cdr (assoc A dist)))

;Промяна на стойността за връх A - връша променения асоциативен списък
(define (set-value! A value dist)
  (begin
  (set-cdr! (assoc A dist) value)
  dist))

;помощна - взима списък от двойки и връща най-малката двойка по дясна стойност
(define (minimal L)
  (cond ((null? (cdr L)) (car L))
        ((< (cdar L) (cdr (minimal (cdr L)))) (car L))
        (else (minimal (cdr L)))))

;Функция, която по даден списък от посетени върхове и списък със тегла на върхове, връща най-лекия непосетен връх
(define (minElem visited dist)
  (let ((listToSearch (filter (lambda (n)                             ;създаваме списък от непосетени върхове
                                 (not (contains visited (car n))))
                               dist)))
  (minimal listToSearch)))                                            ;намираме най-лекия от тях
    

;Функция за актуализация на теглата и родителите в МПД. Приема за аргументи: curent - текущ връх; neighbors - списък със необходените му съседи от вида 
;((VERTEX WEIGHT) ... (VERTEX WEIGHT)); dist - списък със теглата на върховете в граф; previous - списък, указващ родителите в МПД на върховете в граф
;Функцията връща наредена двойка от два списъка - актуализираните dist и previous. След това с car може да се изжлече dist, а с cdr - previous
;Актуализацията се състои в това: за всички необходени съседи на върха: Ако цената им е по-голяма от цената на текушия връх + реброто между него и тях, то 
;цената им се променя на по-малката стойност и в previous се вписва, че current e новия им родител
(define (updateWeights! current neighbors dist previous)
  (begin
    (map (lambda (n)                                                                  ;чрез map обхождаме neighbors
           (cond ((> (get-value (car n) dist)                                        
                     (+ (get-value current dist) (cadr n)))                           ;ако цената на наследника > от общата цена на current и реброто
                  (begin
                    (set-value! (car n) (+ (get-value current dist) (cadr n)) dist)   ;променяме стойността на цената в dist
                    (set-value! (car n) current previous )))))                        ;и стойността на родителя в previous
         neighbors)
    (cons dist previous)))                                                            ;връщаме двойката от новите списъци

 
;Проверява дали за елемента А има асоциация в списъка dist
(define (contains visited A)
  (not (equal? (assoc A visited) #f)))

;Дийкстра
;current - текущ връх. В началото е началния връх
;visited - списък от списъци, представящи пътищата от наччалния до всеки обходен връх. В началото съдържа само списък, съдържащ началния елемент
;dist - асоциативен списък, който указва теглото на всеки връх в графа. В началото за началния ст-стта е 0, а за останалие - безкр.
;prev - асоциативен списък, който указва родителя в МПД на всеки връх в графа. В началото за началния ст-стта е себе си, а за останалите - безкр.
;end - краен връх
;G - граф
(define (dijkstra current visited dist prev end G)
  (if (= (length visited) (length G))                                            ;ако броя обходени върхове е равен на броя върхове в графа, т.е. приключил е алгоритъма
      (cons (get-value end dist) (reverse (assoc end visited)))                  ;връщаме пътя във visited, който води до end, със добавено отпред теглото на пътя
      (let ((neighbors (cdr(assoc current G))))                                  ;(cdr (assoc 'A G)) е списък от наследниците на current - текущия връх
        (let ((unvisited-neighbors (filter (lambda (n)                           ;съкращаваме списъка до всички необходени наследници
                                             (not (contains visited (car n))))
                                           neighbors)))
          (let ((updated (updateWeights! current unvisited-neighbors dist prev)));наредена двойка от обновените списъци за тегло и родител
            (let ((new-dist (car updated)))                                      ;обновен списък за тегла на върховете
              (let ((new-prev (cdr updated)))                                    ;обновен списък за родители на върховете
                (let ((new-vertex (car (minElem visited new-dist))))             ;новия връх се определя като върха с най-ниско тегло
                  (dijkstra new-vertex                                           ;викаме функцията рекурсивно с новия връх
                            (append visited (list (cons new-vertex (assoc (get-value new-vertex prev) visited)))) ;към пътищата до посетени върхове добавяме новия път - като разширяваме пътя на неговия родител с него
                            new-dist                                             ;вече имаме нови тегла
                            new-prev                                             ;нови родители
                            end                                                  ;крайния връх е постоянен
                            G)))))))))                                           ;също и графа

;Извикваща функция - по начален и краен връх и граф връща най-евтиния път
(define (lightest start end G)
  (dijkstra start (list(list start)) (init-dist start G) (init-previous start G) end G))






;Главна програма
(define (min-path start end net)
  (if (or (equal? #f (assoc start G))         ;проверка дали и двата върха са в графа
          (equal? #f (assoc end G)))  
      #f
      (let ((tmp (bfs-help start end net)))   ;най-късия път
        (if (null? tmp)                       ;проверка дали е празен списъкът, т.е. дали има път
            #f
            (cons                             ;създаване на наредената двойка
             (lightest start end net)      
             (list (cons (path-length tmp net) tmp))))))) ;чрез допълнителната функция определяме теглото на пътя и го залепяме отпред

;графът от примера
(define G '((A (B 6) (C 5)) (B (D 8)) (C (A 1) (D 1)) (D (C 7) (E 5) (W 3)) (E (A 11) (D 1) (F 10)) (F) (W)))
 
;тестове
(min-path 'A 'F G)
(min-path 'A 'Z G)
(min-path 'W 'E G)
(min-path 'E 'A G)
;(min-path 'E 'E G)