A simple Haskell server
=======================
An unfinished study comparing JavaScript and Haskell servers.

## A JavaScript primer
Here's a node.js HTTP server, taken from the [node.js homepage](https://nodejs.org):
```javascript
var http = require('http');
http.createServer(function(req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(1337, '127.0.0.1');
console.log('Server running at http://127.0.0.1:1337/');
```

It's simple, small and readable. It gets even more readable if you put it like
so:
```javascript
var http = require('http');

function handleRequest(req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}

http.createServer(handleRequest).listen(1337, '127.0.0.1');
console.log('Server running at http://127.0.0.1:1337/');
```

First comes an import from the `http` module. That's all you need to write it,
which is because the `node.js`' core libraries are really extensive. My
impression, on `node.js`' case of having really broad core libraries, it seems
that there's a certain limitation on how far you can express certain concurrent
scenarios, or computation, really. It's because node.js is a relatively
high-level platform.


The core of the server is the `http.createServer` call. It takes a callback
function of type `(http.IncomingMessage, http.ServerResponse)` to nothing,
which gets called every time a request comes in. The body of the
`handleRequest` function is just a couple of method calls over the
`ServerResponse` instance that gets passed in as an argument.

There's yet another class underneath this API, `http.Server`, you could rewrite
it as:
```javascript
var http = require('http');

var server = new http.Server();

server.on('request', function(req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
});

server.listen(1337, '127.0.0.1');
```

This is a very common idiom in JavaScript. `http.Server` is actually an
instance of `events.EventEmitter`, which is the `node.js` API for JavaScript's
event listening pattern.

I like the example server and that it hides its complexity really well in
really well designed and clean manner. `node.js` style is usually a nice mix of
very object oriented structures, with somewhat functional APIs here and there.

And a lot of really popular modules are very object oriented (whether it shows
or not). In `mongoose`'s case this is very clear, it provides a modeling system
for database entities and the logic related to them. It has support for
attaching instance methods, static methods and more. It even has a plugin
system, which will either work as a `mixin` of some kind or go the monkey
patcher way and have fun with some internal, esoteric hack.

Here's an example "class" (`Model` actually - and here JavaScript gets unique
about inheritance, because this isn't standard prototypical inheritance
underneath) definition using `mongoose`:
```javascript
var mongoose = require()

var userSchema = new mongoose.Schema({
  firstName: [{ type: String }]
  lastName: [{ type: String }]
})

userSchema.methods.fullName = function() {
  return this.firstName + this.lastName;
};

var User = mongoose.model('User', userSchema);
```

`superagent` exposes a builder API, `mocha` uses a `Mocha` god class which uses
several other classes (`Runner`, `Spec`, `Reporter` - which is even abstract -,
etc.). I found this reliance on OOP for this bunch of Node.JS libraries to
contrast in an interesting way to the statement JavaScript is a very _lisp-like_
language - not that it isn't.

- - -
## The Haskell take

Let's look at a Haskell Warp server:
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    putStrLn "Server running at http://127.0.0.1:9000"
    run 9000 app

app :: Application
app _req respond = respond $ responseLBS status200 headers body
 where
   headers = [ ("Content-Type", "text/plan")]
   body = "Hello World"
```

It's not so far from the Node.js server. `app` is like our server callback in
Node.js. It takes a `_req` object and a `respond` callback function.

In way, we could make it more _readable_ by using similar naming to the Node.js
example:
```haskell
main = do
    putStrLn "Server running at http://127.0.0.1:9000"
    run 9000 handleRequest

handleRequest _req respond = respond $ responseLBS status200 headers body
 where
   headers = [ ("Content-Type", "text/plan")]
   body = "Hello World"
```

Even though Haskell is a super typed language, we can omit the type signatures.
It'll just infer them.

**TODO** Keep writting about this.

## License
Everything under this repository is licensed under the GPLv2 license.
