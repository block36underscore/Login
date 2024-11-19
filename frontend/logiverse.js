export default {
  name: "login.block36.gay",
  admin: "block36_",
  feed: "http://127.0.0.1:4242/getUsers"
};

// Login instances copied from svenlaa's loggo instance 
// https://github.com/Svenlaa/svenlaa.com/blob/main/playground/loggo/logiverse.js
// There doesn't seem to be any licensing info, but that's probably fine

export const config = {
    version: '0.0.1',
    instance_name: 'login.block36.gay',
    instance_admin: 'block36_',
    endpoints: {
      update: 'http://127.0.0.1:4242/update',
        login: 'http://127.0.0.1:4242/login',
        delete: 'http://127.0.0.1:4242/delete',
        ban: 'http://127.0.0.1:4242/ban',
    },
    instances: [
        {
            name: 'todepond.com',
            feed: 'https://todepond-lablogingetusers.web.val.run',
            gifs: {
                berd: {
                    src: 'https://www.todepond.com/image/berd.gif',
                    alt: 'A sparkly bird gif',
                },
                bot: {
                    src: 'https://www.todepond.com/image/bot.gif',
                    alt: 'A sparkly robot gif',
                },
                tode: {
                    src: 'https://www.todepond.com/image/tode.gif',
                    alt: 'A sparkly toad gif',
                },
            },
        },
        {
            name: 'svenlaa.com',
            feed: 'https://svenlaa-lablogingetusers.web.val.run',
            // update: "https://svenlaa-labloginupdatestatus.web.val.run",
            // login: "https://svenlaa-lablogin.web.val.run",
            // delete: "https://svenlaa-lablogindeleteaccount.web.val.run",
            // ban: "https://svenlaa-labloginbanuser.web.val.run",
        },
        {
            name: 'evolved.systems',
            feed: 'https://evol-lablogingetusers.web.val.run',
            // update: "https://evol-labloginupdatestatus.web.val.run",
        },
        {
            name: 'rossilaz.xyz',
            feed: 'https://mittzy-loginredux_getusers.web.val.run',
        },
        {
            _comment: 'client on github pages @ https://cute-catgirl.github.io/login/',
            name: 'maemoon.me',
            feed: 'https://maemoon-lablogingetusers.web.val.run',
        },
    ],
};
