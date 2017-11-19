
# シリアライザの実装

- シリアライザをミューテックスと呼ばれる同期メカニズムでもって実装する
- ミューテックスは２つの演算を持つオブジェクト
    ミューテックスは
    1. acquire(獲得)される
    2. release(解放)される
- ミューテックスが獲得(acqure)されると、他は解放されるまで獲得することはできない
- SICPではそれぞれのシリアライザは１つの関連付けられたミューテックスを持つ
- 手続きpが与えられるとシリアライザは、
    1. ミューテックスを獲得し
    2. pを実行し
    3. ミューテックスを開放する
    という手続き(p')を返す。
- シリアライザによって生成された手続きは一度に１つだけ実行することができる、ということを保証する
    - このことが保証されなければならない特性である


実装
---

```scheme
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'relase)
          val))
      serialized-p)))
```
- ミューテックスは変更可能なオブジェクトでtrueかfalseを値として持つ
    - １要素のリストを用いて実装する。これをここではcellと呼ぶ。
    - false ... ミューテックスは獲得可能
    - true ... ミューテックスは獲得されていて、獲得するにはまたなければならない。


---

```scheme
(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell) #t (begin (set-car! cell #t) #f)))

```

- mutexのコンストラクタ




---

#### Ex. 3.46
次の二つを行う。
    1. cellのチェック
    2. cellの更新

p1が1. を行い、p2が1. 2.を行い、p1が2. を行えばp1,p2が同時にミューテックスを確保できる。

#### Ex. 3.47

3_4-1.rktに書いた。




---
---

## Deadlock

　

