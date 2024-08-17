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

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Golang where you will learn about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
