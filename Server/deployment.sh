#!/bin/bash
python manage.py makemigrations 
python manage.py migrate
echo "deployment.sh success"