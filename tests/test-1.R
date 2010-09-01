expect_that(cleanNames(data.frame(State="Michoacan de Ocampo")),
            equals("Michoacan"))
expect_that(cleanNames(data.frame(State="Querétaro de Arteaga")),
            equals("Querétaro"))

expect_that(convertToDate(0), equals(as.Date("2000-01-15")))
expect_that(convertToDate(10), equals(as.Date("2000-11-15")))
expect_that(convertToDate(24), equals(as.Date("2002-01-15")))

expect_that(convertToDate(10), is_a("Date"))

