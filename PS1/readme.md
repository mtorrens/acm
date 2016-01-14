## Advanced Computational Methods - Problem Set #1

### Exercise 1

  * The generated data set is limited to either two or three dimensions for simplicity. This can be relaxed but be aware of the [curse of dimensionality](https://en.wikipedia.org/wiki/Curse_of_dimensionality) when accounting for how many points will actually fall inside the hypersphere.

  * Bear in mind that the results will be generated in R's current working directory: its default directory if not otherwise specified. In any case, the execution of the function will display on the console the full path of the written files.

### Exercise 2

  * It is unclear to me if this exercise requires to write a function or have only the executable code in the script. Thus, the script contains a function that carries out the exercise and it executes it at the end, to cover both possibilities.

  * The function has a logical argument called `dashed`, which specifies whether the generated plot displays the discriminant functions dashed in the part where the function has no effect. If the argument is set to `FALSE` then the plot shows the functions fully colored on its domain. If it is set to `TRUE` (default value), the plot is built in a more manual manner. The code is uglier as it requires to find the intersection points of the functions and plot them accordingly, but the output is nicer. However, this parameter is disabled if the function is unsure on which are the applying boundaries (which parts of the functions should be dashed).

### Exercise 3

  * The app is under construction. This is not due by January 15th.
