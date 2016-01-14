## Advanced Computational Methods - Problem Set #1

### Exercise 1

  * The generated data set is for simplicity limited to either two or three dimensions. This can be relaxed but be aware of the [curse of dimensionality](https://en.wikipedia.org/wiki/Curse_of_dimensionality) when accounting for how many points will fall inside the hypersphere.

  * Bear in mind that the results will be generated in R's working directory: its default directory if not otherwise specified. The execution of the function will display the full path of the written files.

### Exercise 2

  * It is unclear to me if this exercise requires to write a function or have only the executable code in the script. Thus, the script contains a function that carries out the exercise and it executes it at the end, to cover both possibilities.

  * The function has a logical argument called `dashed`, which specifies whether the generated plot displays the discriminant functions dashed in the part where the function is not used. If the argument is set to `FALSE` then the plot shows the functions fully colored on its domain. If it is set to `TRUE`, the plot is built in a more manual manner. The code is uglier as it requires to find the intersection points of the functions and plot them accordingly, but the output is nicer. Also, this parameter is disabled if the function is unsure on which are the applying boundaries (which part of the function should be dashed).

### Exercise 2

  * The app is under construction.
