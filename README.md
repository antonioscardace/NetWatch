# NetWatch • Utilities Health Monitor

[![CodeFactor](https://www.codefactor.io/repository/github/antonioscardace/netwatch/badge)](https://www.codefactor.io/repository/github/antonioscardace/netwatch)
[![License](https://img.shields.io/github/license/antonioscardace/netwatch.svg)](https://github.com/antonioscardace/netwatch/blob/master/LICENSE)
[![Open Issues](https://img.shields.io/github/issues/antonioscardace/netwatch.svg)](https://github.com/antonioscardace/netwatch/issues)
[![CI for Testing](https://github.com/antonioscardace/NetWatch/actions/workflows/ci-test.yml/badge.svg)](https://github.com/antonioscardace/NetWatch/actions/workflows/ci-test.yml)

The project is a health checker. It aims to check in real-time the health of each observed local utility.<br/>
The utility can be a Docker Container, a Machine, or a Network Device in your local network.<br/>
The monitor notifies the utility referent if it changes its state (i.e. **goes offline** or **comes back online**).<br/>
The alert can be a Slack message, an Email, or a Telegram message like these:

<img src="/docs/snaps/telegram.png" alt="Telegram" width="500px"/>
<img src="/docs/snaps/email-online.png" alt="Telegram" width="500px"/>

An accurate report can be found in [/docs/report.pdf](/docs/report.pdf)<br/>
This project was created as an exam project to test a set of skills, including:

+ Knowledge of Microservices 
+ Knowledge of Design Patterns
+ Knowledge of Java, Maven, and Spring Boot
+ Knowledge of Messaging Systems (RabbitMQ) and their Patterns
+ Knowledge of Git & GitHub

I also had the opportunity to practice the following skills:
+ Knowledge of ORMs (Hibernate)
+ Knowledge of Redis
+ Use of the Static Analysis Tools (CodeFactor, Snyk Code, and Sonar Qube)

## System Infrastructure

Each microservice is contained in a Docker Container.<br/>
The only reachable containers in the Docker Network are Frontend, API Gateway, and Grafana.<br/>
The UML of the internal structure of each microservice is stored in [/docs/uml/](/docs/uml/)<br/>

<img src="/docs/images/infrastructure.svg" alt="Infrastructure"/>

Main Notes:

- _RabbitMQ_ provides an Exchange implementing the Publish-Subscribe Design Pattern.
- The HTTP connection between _Monitor_ and _Data Manager_ is a Keep-Alive (persistent) connection.
- _Monitor_ implements the Factory-Method Design Pattern for the _request_ module.
- _Notification_ implements the Factory-Method Design Pattern for the _sender_ module.

## Getting Started

* Get any set of valid credentials or tokens (e.g. for Telegram) to send notifications.
* Download and install [Docker](https://docs.docker.com/get-docker/).

```sh
   $ git clone https://github.com/antonioscardace/NetWatch.git
   $ cd YOUR_PATH/NetWatch/
   $ bash run.sh
``` 

### Useful Links

Container | URL | Description
----- | ------- | -------
grafana-charts | [https://localhost:8800/](https://localhost:8800/d/2xZ6SccSz/netwatch-logs?orgId=1&refresh=5s&from=now-24h&to=now) | Grafana Logs
rabbitmq | http://localhost:15672/ | RabbitMQ UI
