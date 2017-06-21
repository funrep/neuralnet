# neuralnet

> An artificial neural net in Haskell.

A basic implementation of a feedforward ANN with backpropegation.

## Try it out

The function `test` trains a network to learn the XOR operator.

```
$ stack ghci
...
> net <- test
> runNet [1,0] net
0.969682524723555
```

## License

Licensed under BSD3.