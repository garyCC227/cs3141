- where, do, let, case..of is indent sensitive
main =
    let x = 5
        y = 6
        in print(x+y)

    or you can us {}, so we can ignore indent
    let {x=5; y=6} in print(x+y)
