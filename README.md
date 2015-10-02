## [Mastering Data Analysis with R](http://bit.ly/mastering-R)

This repository includes the example R source code and data files for the above referenced book published at Packt Publishing in 2015. 

The above R files are identical to the R code examples found in the book except for the leading `>` and `+` characters, which stand for the prompt in the R console. As the book included both the R commands and output, the prompt was also shown in the examples so that the reader can easily distinguish the calls from the returned values:

```r
> set.seed(42)
> data.frame(
+  A = runif(2),
+  B = sample(letters, 2))
A B
1 0.9148060 h
2 0.9370754 u
```

In this repository, both the output and the prompt were intentionally removed here along with arbitrary line-breaks, so that you copy and paste the R expressions to the R console in a more convenient and seamless way.

The code chunks are grouped by the printed pages of the book. Two hash signs at the beginning of a line stands for a page break, while an extra empty line between the code chunks represents one or more paragraphs in the original book between the examples for easier navigation. Sometimes extra instructions starting with a double hash are also provided on how to run the below expressions.

Please find more information on the book at http://bit.ly/mastering-R and you can contact me on [Twitter](https://twitter.com/daroczig) and [GitHub](https://github.com/daroczig) in case of any question or feedback.

I hope you will enjoy and find useful this book!
