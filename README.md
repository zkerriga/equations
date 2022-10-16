# equations
The program solves equations (the quadratic degree is maximal)

### Description

This project is an attempt to assemble a program in Scala 3 that will validate and solve quadratic equations.
```
Write equation: x^2 - 14x + 1 = 41
```

At the same time, the program splits the input string into chunks and tries to validate and describe all the mistakes made:
```
4x^12 - 0 +  + 19^-x x + 12 * x^-1  =  31x + * 3 -^4
            ^    ^                           ^    ^
                                                  Unexpected exponent of the summand
                                             Unexpected variable of the summand
                 Unexpected exponent of the summand
            Unexpected end of an expression

```

Unfortunately, the project remained incomplete, and we can only see the simplified form of the equation and its degree:
```
Write equation: 5x^13 - x + 4X ^ 13 + x^1 = 9  * x^13 - 200x + 1
Raw equation: 5X^13 - X + 4X^13 + X - 9X^13 + 200X - 1 = 0
Reduced form: -1 + 200X = 0
Polynomial degree: 1
```