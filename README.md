dostoevsky
==========

[![Build Status](https://travis-ci.org/Raphexion/dostoevsky.svg?branch=master)](https://travis-ci.org/Raphexion/dostoevsky)

Small Volume Persistent Event Sourcing / PubSub Framework.

Birds view
----------

Imagine that you have Car Dealership (DShip) that sells cars.
They want to tell the world about the cars that are for sale.
They use a Classified Advertisements Website called CAd.
Unfortunatly, CAd has have some stability problems lately.
The site crashes and loses its data on a bi-weekly basis.
After CAd crashes, DShip are forced to re-list all of their cars.
This is a big pain.

Imagine a intermediate (Dostoevsky). It has the following features:

DShip sends (HTTP POST) to /pub/

	{"NEW_CAR": {"type": "Ford Mondeo", "price": 10000, "id": 1234}}

to Dostoevsky, which saves it.

DShip can send many more.

	{"NEW_CAR": ...}
	{"NEW_CAR": ...}
	...
	{"NEW_CAR": ...}

CAd on the other hand wants to subscribe to a topic. However,
they don't do it the normal broker way (RabbitMQ). They want
to get a REST POST message to a known endpoint.

CAd sends (HTTP POST) /sub/

	{"NEW_CAR": {"url": "http://1.2.3.4:8080/foo", "history": true}}

This will cause Dostoevsky to send all of the DShip items to the endpoint,
one after another.

This means that a CAd service can rebuild its state from the history.
