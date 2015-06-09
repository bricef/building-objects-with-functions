Next steps
==========

* Get web editor in a state where it can be used to develop the talk itself.

Q: Is there a way I could dynamically build the talk from the editor?
A: you'd need an env-saver service

String save is pretty straight forward -> login? Unique id? Firebase?

[Save]-> Prompt for email -> associates the email with the session on firebase -> sends email with session link

Firebase:
{SessionID: {
    history:[{timestamp:123456789,env:<env>},<...>],
    claimed-by: <email>
}}

How about me as editor?

Is the state of the talk also editable? what's the access control?
Talk is just a series of elements, no reason it can't be persisted in the same way as the environment. Perhaps too far for now :)