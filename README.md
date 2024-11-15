# Smessenger

`Smessenger` is a real-time messaging service built on **ejabberd** (a robust XMPP server). This setup is configured with **PostgreSQL** as the database, enabling federated messaging, client-server communication, and web-based management via a web admin interface.

## Installation

### Prerequisites
Ensure the following are installed on your machine:
- [Docker](https://docs.docker.com/get-docker/)
- [Docker Compose](https://docs.docker.com/compose/install/)

### Setup

1. Clone the repository:
   ```bash
   git clone git@github.com:plavno-plavno/smessenger.git
   ```
2. Navigate into the project directory:
    ```
    cd smessenger
    ```
3. Copy ejabberd configuration file:
    ```
    cp ejabberd.yml.example ejabberd.yml
    ```
4. Start the Docker containers:
    ```
    docker compose up
    ```
    This command will set up and start both the ejabberd and PostgreSQL services as defined in the docker-compose.yml file.


## Usage
After running docker compose up, the following URLs and ports are available:

- Web Admin Interface: http://localhost:5280/admin
- To connect an XMPP client, use localhost as the server address, with the default XMPP client port `5222`.

## First start 
On the first start `docker compose up` executes `setup-ejabberd.sh` to pre-configure some default users, rooms, roasters:
### Users
- `user1@localhost` password `user1password`
- `user2@localhost` password `user2password`
- `user3@localhost` password `user3password`
- `user4@localhost` password `user4password`
- `user5@localhost` password `user5password`

All users listed above have been added to the pre-configured room __chatroom1@localhost__
with next affilations:
- `user1@localhost` affilation `member`
- `user2@localhost` affilation `member`
- `user3@localhost` affilation `admin`
- `user4@localhost` affilation `owner`
- `user5@localhost` affilation `outcast` (=banned)

Also `user1` and `user2` by default added to each other __roasters__
### Global admin
`admin@localhost` password `adminpassword`
#### Note
if you want to rerun `setup-ejabberd.sh`(reset users) the only option for now is to remove volumes:
  - `docker compose down -v`
  - `docker compose up` 
