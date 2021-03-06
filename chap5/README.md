### 5.1.5 命令の要約

```<input_i>``` は ```(reg <register-name>)``` か ```(const <constant-value)```とする.

- ```(assign <register-name> (reg <register-name>))```
    - ```(assign a (reg b))```
      register a に register bの値 を代入.
- ```(assign <register-name> (const <constant-value>))```
  - ```(assign a (const 1))```
  - 今まではconstant-valueとして数値しか扱ってこなかったが，これからは文字列・記号・リストなども扱っていく.
- ```(assign <register-name> (op <operation-name>) <input_1> ... <input_n>)```
  -  ```(assign t (op rem) (reg a) (reg b))```

- ```(perform (op <operation-name>) <input_1> ... <input_n>)```
  - ```(perform (op print) (reg a))```

- ```(test (op <operation-name>) <input_1> ... <input_n>)```
  - ```(test (op =) (reg b) (const 0))```

- ```(branch (label ⟨label-name⟩))```
  - ```(branch (label gcd-done))```
- ```(goto (label ⟨label-name⟩))```
  - ```(goto (label gcd-loop)))```
    テストが偽なら, 制御器は列での次の命令を続ける. そうでなければ, 制御器はラベルの後の命令から続ける.


- ```(assign ⟨register-name⟩ (label ⟨label-name⟩))```
  - ``` (assign continue (label after-gcd-1))```
- ```(goto (reg ⟨register-name⟩))```
  - ``` (goto (label gcd))```


- ```(save ⟨register-name⟩)```
  - ```(save continue)```
  - ```(save n)```
- ```(restore ⟨register-name⟩)```
  - ```(restore continue)```
  - ```(restore n)```


## 5.2 レジスタ計算機シミュレータ

本節では, 設計したレジスタ計算機をテストするために，レジスタ計算機のシミュレータを構成する.

シミュレータは４つのインターフェイスの手続きをもつSchemeプログラムである.

１つはレジスタ計算機の記述を使い，計算機モデルを構成子，他の３つは我々にモデルを操作し，計算機をシミュレートさせる.

- ```(make-machine <register-names> <operations> <controller>)```
  : 与えられたレジスタ，演算，及び制御器を持つ計算機のモデルを構成し，それを返す.
- ```(set-register-contents! <machine-model> <register-name> <value>)```
  : 与えられた計算機のシュミレートされるレジスタに値を格納する.
- ```(get-register-contents <machine-model> <register-name> <value>)```
  : 与えられた計算機のシュミレートされるレジスタの内容を返す.
- ```(start <machine-model>)```
  : 与えられた計算機の実行をシミュレートする. 制御列の先頭から実行開始し，その列の最後に達した時に停止する.

例:

```scheme
(define gcd-machine
  (make-machine
   '(a b t) ; レジスタ名のリスト
   (list (list 'rem remainder) (list '= =)) ; 演算名と演算を実装するScheme手続きの対 のリスト
   '(test-b ; ラベルと計算機命令のリストとして制御器を規定する.
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))
```


```scheme
; gcd-machine のレジスタ a に 206 を格納
> (set-register-contents! gcd-machine 'a 206)

done

; gcd-machine のレジスタ b に 40 を格納
> (set-register-contents! gcd-machine 'b 40)

done

; gcd-machine のシミュレーションを実行
> (start gcd-machine)

done

; gcd-machine のレジスタ a に格納されている値 a を取る
> (get-register-contents gcd-machine 'a)

2
```


### 5.2.1 計算機モデル

```make-machine```が生成する計算機のモデルは, ３章で開発したメッセージパッシング技法を使い，局所状態を持つ手続きで表す.

モデルを構築するには，```make-machine```がすべてのレジスタ計算機に共通な計算機モデルの部品を構成するため，手続き```make-new-machine```を呼び出す.

```make-new-machine```で構成される基本的計算機モデルは, 本質的にいくつかのレジスタとスタックの容器(?)と，制御器の命令を１つずつ処理する実行機構である.

```make-machine```はこの基本モデルを，（メッセージを送ることで）定義しようとする特定の計算機のレジスタ，演算及び制御器を含むように拡張する．


```scheme
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
```

- **レジスタ**

```scheme
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))
```

- **スタック**

```scheme
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push) ; 引数有り, 関数を返す
            ((eq? message 'pop) (pop)) ; 引数なし, その場で実行し, 返り値を返す.
            ((eq? message 'initialize) (initialize)) ; 引数なし, その場で実行し, 返り値を返す.
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))
次の手続きは, スタックにアクセスするのに使う.

(define (pop stack)
  (stack 'pop))


(define (push stack value)
  ((stack 'push) value))
```

- **基本計算機**

- ```flag```  
  レジスタ. 
- ```pc```  
  レジスタ.
- ```allocate-register```
- ```lookup-register```
- ```instruction execution procedure```


```scheme
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
```




```scheme
(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))


(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)


(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

```


未実装.
- instruction-execution-proc
- assemble
  - ``make-machine``の内部で使われている.
  - controller-text(ラベルなどが並んでるやつ)とmachineを引数として，実際に実行できる命令のリストを返す.


### 5.2.2 アセンブラ


コントローラの式 -- アセンブラ --> 機械語



```scheme

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))


(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))


(define (instruction-text inst)
  (car inst))


(define (instruction-execution-proc inst)
  (cdr inst))


(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))
```




```scheme

; makeoperation-exp
; make-

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; assignの実行手続き
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

```


## 5.3 

```
(vector-ref <vector> <n>)
(vector-set! <vector> <n> <value>)
```


## 5.4

5.1節では，単純なSchemeプログラムをレジスタ計算機の記述へ変換する方法をみた．
5.4節では，4.1節の超循環評価器(インタプリタ)をレジスタ計算機の記述へ変換する．

>The explicit-control evaluator that we develop in this section shows how the underlying procedure-calling and argument-passing mechanisms used in the evaluation process can be described in terms of operations on registers and stacks. In addition, the explicit-control evaluator can serve as an implementation of a Scheme interpreter, written in a language that is very similar to the native machine language of conventional computers. The evaluator can be executed by the register-machine simulator of section 5.2. 


