var express = require('express');
var bodyParser = require('body-parser');
var cors = require('cors');

var app = express();

var users = [];

app.use(bodyParser.json());
app.use(cors());

app.get('/users/:id?', (req, resp) => {
    if (req.params.id) {
        const index = users.findIndex(user => user.id === parseInt(req.params.id));
        if (index !== -1) {
            resp.status(200).send(users[index]);
            return;
        }
        resp.statusCode = 404;
        return resp.json({ error: 'Not found' });
    }
    resp.status(200).send(users);
});

app.post('/users', (req, resp) => {
    if (req.body) {
        let user = req.body;
        user.id = parseInt(Math.random() * 10000);
        users.push({ id: user.id, name: user.name, lastName: user.lastName, email: user.email });
        resp.status(201).send(user);
        return;
    }
    resp.statusCode = 400;
    return resp.json({ error: 'Bad request' });
});

app.put('/users/:id', (req, resp) => {
    if (req.params.id) {
        const index = users.findIndex(user => user.id === parseInt(req.params.id));
        if (index !== -1 && req.body) {
            let user = req.body;
            let userStored = users[index];
            userStored = {
                id: userStored.id,
                name: user.name ? user.name : userStored.name,
                lastName: user.lastName ? user.lastName : userStored.lastName,
                email: user.email ? user.email : userStored.email
            };
            users[index] = userStored;
            resp.status(200).send(userStored);
            return;
        }
    }
    resp.statusCode = 404;
    return resp.json({ error: 'Not found' });
});

app.delete('/users/:id', (req, resp) => {
    if (req.params.id) {
        const index = users.findIndex(user => user.id === parseInt(req.params.id));
        if (index !== -1) {
            users.splice(index, 1);
            resp.status(200).send();
            return;
        }
    }
    resp.statusCode = 404;
    return resp.json({ error: 'Not found' });
});

app.listen(8888);