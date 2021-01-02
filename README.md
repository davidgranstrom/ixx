ixx - for laptop ensemble
=========================
*David Granström 2012*

## overview

### setup

'ixx' can be played by three or more performers. The speaker setup should
be in a circular formation, where each voice occupy a certain part of the
circle:

    high - left area
    bass - center (front) area
    mid  - right area

Ideally, each performer should have their own speaker. It is also possible to
perform 'ixx' with each voice in stereo. This is preferred over mono if there
are less than six performers participating.

### performance notes
Each voice (high, mid, low) should always play the note events in unison
throughout the piece.  If a note event is accidentally triggered by the
performer, he/she will have to wait for the next event to take place before
advancing any further. There is an **event counter** in the graphical interface
which can be of help in such situations.

## instructions

### preparation

* Configure all loudspeakers to the same output level.

* Route each of the different voices (high, mid, low) to adjacent speakers - in order to form groups of voices.

### performance

0. In case anything has been tested/modified since starting the program ('ixx'),
press `ESC` to do a full reset.

1. Start the piece simultaneously.

3. Follow the instructions which are displayed in the top area of the score
during the piece. Here is an overview of the instructions, chronologically:

    1. *"increase index/cutoff over 7 minutes"* - a gradual increase of the parameters.
    2. *"(percussive events)"*                  - just a notice, try to sync.
    3. *"start to introduce gated note events"* - introduce, don't play them exclusively.
    4. *"only play gated note events"*          - all voices play gated note events.
    5. *"only play clean note events. rapidly decrease index/cutoff"* - all voices play clean note events.
    6. *"end"*                                  - the piece ends.

* When releasing a note event, compensate the time it takes for the envelope
to fully release. The aural result of a note event should match one in the score.

* The increase/decrease of the cutoff/index parameters can be done between or
during a note event. If you choose to increase during a note event, you could
also decrease the same parameter, in order to create a small "swell/bow" over the
note event.

## keybindings

### note events
* clean note event
    * `z` trigger long envelope
    * `x` trigger short envelope

* gated note event
    * `a` trigger long envelope
    * `s` trigger short envelope

* cutoff
    * `arrow right` increase cutoff
    * `arrow left` decrease cutoff

* index
    * `arrow up` increase index
    * `arrow down` decrease index

### general

* `<ENTER>` start/stop

* `<ESC>` reset

* advance score
    * `n` next half-page
    * `p` previous half-page

