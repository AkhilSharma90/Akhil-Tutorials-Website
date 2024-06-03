---
title: "Basic Calculator with Julia"
description: "Julia, a high-level, high-performance programming language, is designed for technical computing"
icon: "code"
draft: false
---

## Code Example: Basic Calculator in Julia

Here’s a simple calculator that performs addition, subtraction, multiplication, and division:

```julia
function calculate(a, b, operation)
    if operation == "+"
        return a + b
    elseif operation == "-"
        return a - b
    elseif operation == "*"
        return a * b
    elseif operation == "/"
        return a / b
    else
        return "Unknown operation"
    end
end

println(calculate(10, 5, "+")) # 15
println(calculate(10, 5, "-")) # 5
println(calculate(10, 5, "*")) # 50
println(calculate(10, 5, "/")) # 2.0
```

This example introduces basic function creation and usage, along with conditional statements.

