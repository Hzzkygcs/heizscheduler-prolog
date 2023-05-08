docker build --tag heizscheduler --progress=plain .
docker run --name heizscheduler -p 80:8000 heizscheduler
