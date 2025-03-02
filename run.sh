docker compose down --volumes

docker compose build redis-idm
docker compose build mysql-idm
docker compose build identity-manager
docker compose build mysql-manager
docker compose build manager
docker compose build redis-monitor
docker compose build monitor
docker compose build rabbitmq
docker compose build notification
docker compose build logstash
docker compose build elasticsearch
docker compose build api-gateway
docker compose build grafana-charts

docker compose up -d