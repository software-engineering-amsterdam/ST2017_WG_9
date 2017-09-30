# Exercise 8

#### Is there a difference between the symmetric closure of the transitive closure of a relation RR and the transitive closure of the symmetric closure of RR?

###### Deliverable: If your answer is that these are the same, you should give an argument, if you think these are different you should give an example that illustrates the difference.

Time spent: 20m

Answer: There is a difference.

###### Argumentation:

Transitive closure is a one-directional extension of a relation R. That means that the closure creates a set of binary relations that go from left to right.

Symmetric closure makes the set two-directional: you can go from left to right but also from right to left.

As an example let's take the following relation

```
R = [(1,2),(2,3)]
```

Its transitive closure is

```
R⁺ = [(1,2),(1,3),(2,3)]
```

Symmetric closure of R⁺ is then:

```
S(R⁺) = [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
```

Symmetric closure of R is

```
S(R) = [(1,2),(2,1),(2,3),(3,2)]
```

And the transitive closure of S(R) is then:

```
S(R)⁺ = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
```

By example we have proven that

```
S(R⁺) ≠ S(R)⁺
```

When we take the `trClos` and `symClos` methods from exercises 5 and 6, then the following operations in Haskell will produce the exactly same results as described above:

```haskell
*Exercise7> symClos $ trClos [(1,2),(2,3)]
[(1,2),(2,1),(1,3),(3,1),(2,3),(3,2)]
>
*Exercise7> trClos $ symClos [(1,2),(2,3)]
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
>
```
