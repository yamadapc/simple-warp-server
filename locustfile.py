from locust import HttpLocust, TaskSet

def index(l):
    l.client.get("/")

class UserBehaviour(TaskSet):
    tasks = {index: 2}

class WebsiteUser(HttpLocust):
    #host = "http://127.0.0.1:1337"
    task_set = UserBehaviour
    min_wait = 5000
    max_wait = 9000
