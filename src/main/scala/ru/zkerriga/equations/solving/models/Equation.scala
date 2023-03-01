package ru.zkerriga.equations.solving.models

import ru.zkerriga.equations.domain.Coefficient

/**
 * We can solve equations with type: `ax^2 + bx + c = 0`
 * @param quadratic
 *   is A coefficient
 * @param linear
 *   is B coefficient
 * @param free
 *   is C coefficient
 */
case class Equation(quadratic: Coefficient, linear: Coefficient, free: Coefficient)
