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
3. Start the Docker containers:
    ```
    docker compose up
    ```
This command will set up and start both the ejabberd and PostgreSQL services as defined in the docker-compose.yml file.
## Usage
After running docker compose up, the following URLs and ports are available:

- Web Admin Interface: http://localhost:5280/admin
- To connect an XMPP client, use localhost as the server address, with the default XMPP client port `5222`.
