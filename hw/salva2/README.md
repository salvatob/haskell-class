# Salvat Parser

Zadání bylo podstatně složitější než jsem čekal (indentace v tokenizátoru), nicméně jsem splinl alespoň minimální zadání...

Přikládám tedy pár zajímavějších fíčur co jsem udělal:

Umí to parsovat věci jako floaty, stringy, uzávorkované výrazy...


```python
x=.2
h="Hello World!"
c='p'
r = 2*(a+b)
```


```js
x <- 0.2;
h <- "Hello World!";
c <- 'p';
r <- 2 * a + b;
```

Moc hezky to hlídá nekonzistentní indentaci
```python
def googoo(l):
   if g+l-looo :
             if 5:
                                l(5)
             else:      pass
        
             l=oooooo+4
   pass
if __name__ > "__main__":
  if 1:if 2:if 3:if 4:5
l=l(s)
```


```js
function googoo(l) {
    if (g + l - looo) {
        if (5) {
            l(5);
        }
         else pass;
        l <- oooooo + 4;
    }

    pass;
}

if (__name__ > "__main__") {
    if (1) if (2) if (3) if (4) 5;
}

l <- l(s);
```
