
# Use an official Python runtime as a parent image
FROM python:3.10-slim-buster

# Install SWI-Prolog
RUN apt-get update && \
    apt-get install -y swi-prolog && \
    rm -rf /var/lib/apt/lists/*

# Set the working directory to /app
WORKDIR /app

# Copy the requirements file to the working directory
COPY ./python/requirements.txt .

# Install the dependencies
RUN echo $(ls -1 -a)
RUN echo "\n"
RUN  pip install -r requirements.txt --cache-dir /pip-cache

# Copy the rest of the application code to the working directory
COPY . .

# Expose port 8000 for the Django server
EXPOSE 8000

WORKDIR /app/python
RUN python manage.py makemigrations & python manage.py migrate

# Set the command to start the Django server
CMD ["python", "manage.py", "runserver", "0.0.0.0:8000"]
