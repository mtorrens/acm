## Advanced Computational Methods - Problem Set #5

The `cTree` function can train and test. If you need to predict the categories for the test dataset, add a `data.frame` with the same structure as the one in the `data` argument but in the `test` argument (it is `NULL` by default and thus returns in-sample predictions).

The function also returns a third argument containing the structure of the built tree, just for disclosure purposes.

**Note**: the code is quite slow, due to time limitations I have not been able to optimize it with C extensions. Please test gently using datasets of reasonable dimensions. An equivalent version was built replacing `for` loops by `apply`-family type of functions but the computation time was not improved.

**Disclaimer**: The coding style follows [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml).
