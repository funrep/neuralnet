# neuralnet

> An artificial neural net in Haskell.

A basic implementation of a feedforward ANN with backpropagation.

## Try it out

The `Numeric.ANN.XOR` modules trains a network to learn the XOR operator.

```
$ stack ghci
...
> import Numeric.ANN.XOR
> net <- xorNet
> runNet [1,0] net
0.969682524723555
```

## License

Licensed under BSD3.