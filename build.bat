docker build --tag heizscheduler --progress=plain .
docker stop heizscheduler || true && docker rm heizscheduler || true
docker run --name heizscheduler -p 80:8000 heizscheduler
