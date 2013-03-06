// ===========================================================================
// Title         : ixx
// Description   : piece for laptop ensemble
// Version       : 1.1
// Copyright (c) : David Granstrom 2012
// ===========================================================================

IXX {

    var <register, <numChannels, <randSeed;
    var <cfreqBus, <indexBus, <mvolBus;

    var <server, <durations;
    var mainGroup, ixxGroup, fxGroup, noiseGroup;
    var busses, gateBus, mainBus, mainNoise, noiseBus, accBus;
    var defName, defCount, durMul, noiseOct, addNoise, noiseRunning, addAccent, sequence, curVol;
    var ixxGui, ixxTest, sTime;

    *new {|register='mid', numChannels=1, randSeed=1769|
        ^super.newCopyArgs(register, numChannels, randSeed).init;
    }

    init {
        if(numChannels>2 or:{numChannels<1}) { "Only mono or stereo output is supported!".throw };
        switch(register)
        { 'high' } {
            {
                Post << "(( IXX )) register: " << register.asCompileString
                << ", numChannels: " << numChannels << Char.nl;
            }.defer(1.5)
        }
        { 'mid' } {
            {
                Post << "(( IXX )) register: " << register.asCompileString
                << ", numChannels: " << numChannels << Char.nl;
            }.defer(1.5)
        }
        { 'low' } {
            {
                Post << "(( IXX )) register: " << register.asCompileString
                << ", numChannels: " << numChannels << Char.nl;
            }.defer(1.5)
        }
        {
            "Not a valid register! Choose from: \'low\', \'mid\' or \'high\'".throw;
        };
        Server.default = server = Server.local;
        curVol       = 1;
        defCount     = 0;
        sTime        = 0;
        addNoise     = false ! 2;
        noiseRunning = false;
        addAccent    = false;
        server.waitForBoot {
            busses = [
                mainBus   = Bus.audio(server,numChannels),
                gateBus   = Bus.audio(server,numChannels),
                mainNoise = Bus.audio(server,numChannels),
                accBus    = Bus.audio(server,numChannels),
                noiseBus  = Bus.audio(server,numChannels),
                indexBus  = Bus.control(server,1).set(0.5),
                cfreqBus  = Bus.control(server,1).set(500),
                mvolBus   = Bus.control(server,1).set(curVol)
            ];
            server.sync;
            ixxGroup   = Group.new;
            noiseGroup = Group.after(ixxGroup);
            fxGroup    = Group.after(noiseGroup);
            mainGroup  = Group.after(fxGroup);
            server.sync;
            this.createMaterial;
            this.makeDefs;
            server.sync;
            server.bind {
                Synth.head(fxGroup, \ixx_reverb);
                Synth.head(fxGroup, \ixx_accReverb);
                Synth.head(fxGroup, \ixx_gate);
                Synth.head(mainGroup, \ixx_main, [\amp, mvolBus.asMap]);
            };
            this.gui;
        };
        CmdPeriod.doOnce { this.free };
    }

    createMaterial {
        var phrase, rest, transp;
        // use to generate score
        // negative values == rests
        rest = Pseed(randSeed, Pwrand([ -9, -18 ], [0.7,0.3],inf)).asStream;
        durations = [
            [ 0, 0, 3, 6, 0, 6, 3, 6 ],    // high
            [ 3, 6, 0, 6, 0, 0, 6, 3 ],    // mid
            2 * [ 0, 3, 6, 0, 0, 6, 3, 0 ] // low
        ].collect{|x| x.collect{|t| if(t<1) { rest.next } { t + 6 } } };
        phrase = [ 56, 57, 68, 64 ];
        transp = Pstutter(phrase.size-1, Pseq([ 0, 7 ], inf));
        // assign pitch sequence to register
        sequence = switch(register)
        { 'high' } {
            durMul  = 1;
            noiseOct = -12;
            defName = \ixx_high;
            (Pseq(phrase + 12, inf, 1) + transp).asStream;
        }
        { 'mid' } {
            durMul  = 1;
            noiseOct = 0;
            defName = \ixx_2;
            (Pseq(phrase, inf) + transp).asStream;
        }
        { 'low' } {
            durMul  = 1.25;
            noiseOct = 12;
            defName = \ixx_bass;
            Pseq((phrase++49) - 12, inf, 1).asStream; // + C#
        };
    }

    makeDefs {
        SynthDef(\ixx_high, {
            |
                freq   = 55,
                index  = 0.5,
                amp    = 0.5,
                hpfreq = 40,
                lpfreq = 2500,
                out    = 0,
                gate
            |
            var num = 2;
            var m = LocalIn.ar(num),
            e  = \env.kr(Env.newClear(4).asArray),
            ee = EnvGen.kr(e,gate,doneAction:2);
            m  = {|i|
                SinOsc.ar(
                    (freq*(i+1))+LFDNoise3.kr(1/10, 0.75),
                    SinOsc.ar(
                        (2.001*(freq+LFDNoise3.kr(1/10, 0.75)))/(i+1),
                        DelayC.ar(
                            m[i]*LFDNoise3.kr(1/30).range(0.75,1.0+(index/4)),
                            0.1,
                            LFDNoise1.kr(1/rrand(4.0,8.0)).range(0.05,0.08)
                        )
                    ) * index
                );
            }.dup(num);
            LocalOut.ar(m);
            m = if(numChannels<2) {
                HPF.ar(m.mean,hpfreq);
            } {
                HPF.ar(Splay.ar(m.scramble),hpfreq);
            };
            m = RLPF.ar(m,ee.exprange(lpfreq/2, lpfreq).clip(20,20000));
            m = BPeakEQ.ar(m,172,0.7,8.neg);
            m = BPeakEQ.ar(m,3262,0.8,9.neg);
            Out.ar(out, ee*((m*0.5)*(AmpComp.kr(freq)*amp)));
        }, 0.1!5).add;

        // numPartials
        [ 2, 3, 4, 7 ].do{|num|
            SynthDef(\ixx_ ++ num, {
                |
                    freq   = 55,
                    index  = 2,
                    amp    = 0.8,
                    hpfreq = 40,
                    lpfreq = 10000,
                    out    = 0,
                    gate
                |
                var m = LocalIn.ar(num),
                e  = \env.kr(Env.newClear(4).asArray),
                ee = EnvGen.kr(e,gate,doneAction:2);
                m  = {|i|
                    SinOsc.ar(
                        (freq*(i+1))+LFDNoise3.kr(1/10, 0.5),
                        SinOsc.ar(
                            (2.001*(freq+LFDNoise3.kr(1/10, 0.5)))/(i+1),
                            DelayC.ar(
                                m[i]*LFDNoise3.kr(1/30).range(0.75,1.0+(index/4)),
                                0.1,
                                LFDNoise1.kr(1/Rand(4.0,8.0)).range(4e-3,0.01)
                            )
                        ) * index
                    );
                }.dup(num);
                LocalOut.ar(m);
                m = if(numChannels<2) {
                    HPF.ar(m.mean,hpfreq);
                } {
                    HPF.ar(Splay.ar(m.scramble),hpfreq);
                };
                m = (0.35*m) + LPF.ar(Saw.ar(freq+LFDNoise3.kr(1/20,0.787)),freq,0.75);
                m = RLPF.ar(m,ee.exprange(lpfreq/2, lpfreq).clip(20,20000));
                m = BPeakEQ.ar(m,172,0.7,8.neg);
                Out.ar(out, ee*(m*(AmpComp.kr(freq)*amp)));
            }, 0.1!5).add;
        };

        SynthDef(\ixx_bass, {
            |
                freq   = 110,
                index  = 10,
                lpfreq = 500,
                sindex = 3.84,
                amp    = 0.8,
                gate, out
            |
            var o = Pulse.ar(freq),
            e  = \env.kr(Env.newClear(4).asArray),
            ee = EnvGen.kr(e,gate,doneAction:2),
            cf = ee.exprange(55,lpfreq);
            o  = [ o,
                PitchShift.ar(o,0.3,1.0001,1e-4,0.2),
                PitchShift.ar(o,0.3,0.995,1e-4,0.2)
            ].sum/3;
            o = o + Pulse.ar(freq/2,0.5,0.1);
            o = o + SinOsc.ar(freq/2,0,0.1);
            o = RLPF.ar(o, cf);
            o = tanh(sin(sindex*o));
            o = if(numChannels<2) {
                GVerb.ar(0.3*o,99,7).mean
            } {
                GVerb.ar(0.3*o,99,7).flop.mean
            };
            o = RLPF.ar(o,(4*cf).clip(20,20000));
            o = o + LPF.ar(tanh(4*o),80,1.0);
            3.do { o = HPF.ar(o,35) };
            o = BLowShelf.ar(o,110,1,6.0.neg);
            o = BPeakEQ.ar(o,3169,0.8,7.neg);
            o = Compander.ar(o,o,16.neg.dbamp,1,1/6,0.1,0.2,1.0);
            Out.ar(out, ee*(amp*Limiter.ar(LeakDC.ar(o),0.9)));
        }, 0.1!5).add;

        if(numChannels<2) {
            SynthDef(\ixx_noise, {|out, amp=0.25|
                var ns  = HPF.ar(ClipNoise.ar,LFDNoise1.kr(1/10).exprange(1200,6500));
                var cl  = Integrator.ar(Impulse.ar(Rand(12.0,17.0)),0.7);
                var cr  = BPF.ar(LFPulse.ar(Rand(7.0,14.0),0,SinOsc.kr(1/Rand(4.0,8.0)).range(0,0.9)), Rand(1000.0,7500.0));
                var atk = 0.25;
                var sus = TChoose.kr(Impulse.kr(0),[9,12]);
                var e   = EnvGen.kr(Env([0,1,1,0],[atk,sus,atk],\sine),doneAction:2);
                ns = [ 0.02*ns, 0.6*cl, 0.3*sin(9*cr), 0.15*HPF.ar(LFNoise2.ar(Rand(1500,5500)),1400) ].mean;
                ns = Integrator.ar(ns,0.815);
                ns = HPF.ar(ns,500);
                Out.ar(out, amp*(e*ns));
            }).add;

            SynthDef(\ixx_bcrush_noise, {|out, amp=0.25|
                var ns  = HPF.ar(ClipNoise.ar,LFDNoise1.kr(1/10).exprange(1200,6500));
                var cl  = Integrator.ar(Impulse.ar(Rand(12.0,17.0)),0.7);
                var cr  = BPF.ar(LFPulse.ar(Rand(7.0,14.0),0,SinOsc.kr(1/Rand(4.0,8.0)).range(0,0.9)), Rand(1000.0,7500.0));
                var atk = 0.25;
                var sus = TChoose.kr(Impulse.kr(0),[9,12]);
                var e   = EnvGen.kr(Env([0,1,1,0],[atk,sus,atk],\sine),doneAction:2);
                ns = [ 0.02*ns, 0.6*cl, 0.3*sin(9*cr), 0.15*HPF.ar(LFNoise2.ar(Rand(1500,5500)),1400) ].mean;
                ns = Integrator.ar(ns,0.815);
                ns = HPF.ar(Latch.ar(ns,Duty.ar(1/Rand(7500.0,12000.0),0,Dseq([0,1],inf))),LFDNoise1.kr(1/10).exprange(200,3500));
                ns = RLPF.ar(ns.round(Rand(0.001,0.098)),7500);
                Out.ar(out, amp*(e*ns));
            }).add;
        } {
            SynthDef(\ixx_noise, {|out, amp=0.25|
                var ns  = HPF.ar(ClipNoise.ar,LFDNoise1.kr(1/10!2).exprange(1200,6500));
                var cl  = Integrator.ar(Impulse.ar(Rand(12.0,17.0!2)),0.7);
                var cr  = BPF.ar(LFPulse.ar(Rand(7.0,14.0!2),0,SinOsc.kr(1/Rand(4.0,8.0!2)).range(0,0.9)), Rand(1000.0,7500.0!2));
                var atk = 0.25;
                var sus = TChoose.kr(Impulse.kr(0),[9,12]);
                var e   = EnvGen.kr(Env([0,1,1,0],[atk,sus,atk],\sine),doneAction:2);
                ns = [ 0.02*ns, 0.6*cl, 0.3*sin(9*cr), 0.15*HPF.ar(LFNoise2.ar(Rand(1500,5500!2)),1400) ].mean;
                ns = Integrator.ar(ns,0.815);
                ns = HPF.ar(ns,500);
                Out.ar(out, amp*(e*ns));
            }).add;

            SynthDef(\ixx_bcrush_noise, {|out, amp=0.25|
                var ns  = HPF.ar(ClipNoise.ar,LFDNoise1.kr(1/10!2).exprange(1200,6500));
                var cl  = Integrator.ar(Impulse.ar(Rand(12.0,17.0!2)),0.7);
                var cr  = BPF.ar(LFPulse.ar(Rand(7.0,14.0!2),0,SinOsc.kr(1/Rand(4.0,8.0!2)).range(0,0.9)), Rand(1000.0,7500.0!2));
                var atk = 0.25;
                var sus = TChoose.kr(Impulse.kr(0),[9,12]);
                var e   = EnvGen.kr(Env([0,1,1,0],[atk,sus,atk],\sine),doneAction:2);
                ns = [ 0.02*ns, 0.6*cl, 0.3*sin(9*cr), 0.15*HPF.ar(LFNoise2.ar(Rand(1500,5500!2)),1400) ].mean;
                ns = Integrator.ar(ns,0.815);
                ns = HPF.ar(Latch.ar(ns,Duty.ar(1/Rand(7500.0,12000.0!2),0,Dseq([0,1],inf))),LFDNoise1.kr(1/10!2).exprange(200,3500));
                ns = RLPF.ar(ns.round(Rand(0.001,0.098!2)),7500);
                Out.ar(out, amp*(e*ns));
            }).add;
        };

        SynthDef(\ixx_pitchNoise, {|out,oct=0,amp=0.1|
            var num  = 5,
            root     = [ 68, 75 ] + oct,
            src      = ClipNoise.ar(0.5), // 2
            freqEven = root.midicps*(1..num).select(_.even),
            freqOdd  = root.midicps*(1..num).select(_.odd),
            bankEven = freqEven.collect{|f|
                var noise = LFDNoise3.ar(1/20);
                BPF.ar(
                    src,
                    f.clip(20,20000)
                    + noise.range(-5,5),
                    noise.range(0.05,0.07)
                )
            },
            bankOdd = freqOdd.collect{|f|
                var noise = LFDNoise3.ar(1/20);
                BPF.ar(
                    src,
                    f.clip(20,20000)
                    + noise.range(-5,5),
                    noise.range(0.05,0.07)
                )
            },
            bank    = (bankEven + bankOdd.reverse),
            modOdd  = freqOdd.collect{|f,i| Resonz.ar(src, SinOsc.ar(0.001*(1/(i+1))).range(f,2*f),0.05) },
            modEven = freqEven.collect{|f,i| Resonz.ar(src, SinOsc.ar(0.0018*(1/(i+1))).range(f,2*f),0.05) };
            bank = if(numChannels<2) {
                (bank + (0.25*(modOdd+modEven))).mean
            } {
                Splay.ar(bank.scramble)
            };
            Out.ar(out, (Line.kr(0,1,6)*amp)*bank);
        },[0,0,4]).add;

        SynthDef(\ixx_quiteNoise, {|db=50,out|
            var ns  = ClipNoise.ar(1!numChannels);
            ns = ns + (0.2*ClipNoise.kr(1!numChannels)*ns);
            ns = if(numChannels<2) {
                LPF.ar(HPF.ar(ns, Rand(10500.0,14500.0)), Rand(15500,17500));
            } {
                LPF.ar(HPF.ar(ns, Rand(10500.0,14500.0!2)), Rand(15500,17500!2));
            };
            Out.ar(out, db.neg.dbamp*ns);
        },[4]).add;

        SynthDef(\ixx_accent, {|out,amp=0.5|
            var ee = EnvGen.kr(Env.perc(0.005,0.7),doneAction:2);
            var o = ee * sin(tanh(SinOsc.ar(35+(ee**4*99),0,2*ee)));
            Out.ar(accBus, amp * o ! 2);
        }).add;

        [
            \ixx_envRand,
            { LFDNoise3.kr(1/Rand(10.0,20.0!numChannels)).range(0,1) },
            \ixx_envSine,
            { FSinOsc.kr(1/Rand(10.0,30.0!numChannels), 1.5pi).range(0,1) }

        ].pairsDo{|name, func|
            SynthDef(name, {|in, out, gate=1, fadeTime=4|
                var inbus = In.ar(in,numChannels);
                var env = SynthDef.wrap(func);
                var xfade = EnvGen.kr(Env([0,1,0],[fadeTime,fadeTime],\sine,1),gate,doneAction:2);
                XOut.ar(out, xfade, env*inbus);
            }).add;
        };

        SynthDef(\ixx_reverb, {
            var i = In.ar(mainBus,numChannels);
            var r = if(numChannels<2) {
                GVerb.ar(0.6*LPF.ar(i,2500), 99, 4, drylevel:0).mean
            } {
                GVerb.ar(0.6*LPF.ar(i,2500), 99, 4, drylevel:0).flop.mean
            };
            var x = XFade2.ar(i, r, -0.5); // 0.22
            ReplaceOut.ar(mainBus, x);
        }).add;

        SynthDef(\ixx_accReverb, {
            var i = LPF.ar(0.6*In.ar(accBus,numChannels), 8500);
            var g = if(numChannels<2) {
                GVerb.ar(i,43,18).sum
            } {
                GVerb.ar(i,43,18).flop.sum
            };
            ReplaceOut.ar(accBus, Limiter.ar(LeakDC.ar(g)));
        }).add;

        SynthDef(\ixx_gate, {
            |
                out    = 0,
                cfreq  = 800,
                thresh = 0.38,
                amp    = 1
            |
            var g = tanh(4*In.ar(gateBus,numChannels)),
            l = LFDNoise1.kr(1!numChannels);
            g = g*Decay2.ar(Dust.ar(l.exprange(24,4)),0.007,l.exprange(0.01,0.5));
            2.do { g = DelayL.ar(g,0.1,Rand(0.05,0.1),1,g*0.5) };
            g = HPF.ar(g, cfreq);
            ReplaceOut.ar(gateBus, g*0.75);
        }).add;

        SynthDef(\ixx_speakerTest, {|out,gate=1|
            var env = EnvGen.kr(Env([0,1,0],[0.05,0.05],\lin,1),gate,doneAction:2);
            Out.ar(out, env*PinkNoise.ar(0.3!numChannels));
        }).add;

        SynthDef(\ixx_main, {
            |
                amp  = 1,
                out  = 0
            |
            var i = In.ar(mainBus,numChannels),
            ns  = In.ar(mainNoise,numChannels),
            acc = In.ar(accBus,numChannels),
            gt  = In.ar(gateBus,numChannels),
            o   = [ i, ns, gt ].sum;
            o   = acc + Compander.ar(o,acc,12.neg.dbamp,1,1/8,0.001,2.5);
            Out.ar(out, amp * Limiter.ar(LeakDC.ar(o)));
        }, [ 0.1 ]).add;
    }

    counter {
        var partIsRunning = false ! 6; // numParts
        var e,n,nn;
        sequence.reset;
        ^Task {
            inf.do{|i|
                var time = ((i-3)+sTime)/60; // minutes (3sec countdown)
                // 0 min
                if(time>0 and:{partIsRunning[0].not}) {
                    server.makeBundle(nil, {
                        e = Synth.tail(noiseGroup, \ixx_envRand, [\out, mainNoise, \in, noiseBus]);
                        n = Synth.head(noiseGroup, \ixx_quiteNoise, [\out, noiseBus]);
                    });
                    if(register=='mid') { defName = \ixx_2 };
                    partIsRunning[0]= true;
                };
                // 3 min
                if(time>2.98 and:{partIsRunning[1].not}) {
                    server.makeBundle(nil, {
                        e !? { e.release };
                        e = Synth.tail(noiseGroup, \ixx_envSine, [\out, mainNoise, \in, noiseBus]);
                        addAccent= true;
                    });
                    if(register=='mid') { defName = \ixx_3 };
                    partIsRunning[1]= true;
                };
                // 4 min
                if(time>3.98 and:{partIsRunning[2].not}) {
                    server.makeBundle(nil, {
                        addNoise[0]= true; // start adding noise synth at this.createSynth
                        nn = Synth.head(noiseGroup, \ixx_pitchNoise, [\out, noiseBus, \oct, noiseOct]);
                    });
                    partIsRunning[2]= true;
                };
                // 6 min
                if(time>5.98 and:{partIsRunning[3].not}) {
                    if(register=='mid') { defName = \ixx_4 };
                    addNoise[0]= false;
                    addNoise[1]= true;
                    nn.set(\amp, 0.17);
                    partIsRunning[3]= true;
                };
                // 8 min
                if(time>7.98 and:{partIsRunning[4].not}) {
                    e !? { e.release };
                    addNoise[0..]= false;
                    addAccent= false;
                    partIsRunning[4]= true;
                };
                // 9 min
                if(time>8.98 and:{partIsRunning[5].not}) {
                    if(register=='mid') { defName = \ixx_2 };
                    partIsRunning[5]= true;
                };
                this.changed(\time, (i-3)+sTime);
                1.wait;
            }
        }
    }

    createSynth {|fx='clean', envShape='short'|
        var env = switch(envShape)
        { 'short' } { Env([0,1,0],[rrand(0.4,0.7),rrand(4.2,5.0)],\sine,1)         }
        { 'long'  } { Env([0,1,0],durMul*[rrand(1.5,1.75),rrand(4.2,6.0)],\sine,1) }
        ;
        // noise
        if(addNoise[0] and:{noiseRunning.not}) {
            if(0.5.coin) {
                Synth(\ixx_noise, [\out, mainBus, \amp, rrand(0.7,0.9)]).onFree{ noiseRunning= false };
                noiseRunning= true;
            }
        };
        if(addNoise[1] and:{noiseRunning.not}) {
            if(0.6.coin) {
                Synth(\ixx_noise, [\out, mainBus, \amp, rrand(0.8,1.0)]).onFree{ noiseRunning= false };
            } {
                Synth(\ixx_bcrush_noise, [\out, mainBus, \amp, rrand(0.8,1.0)]).onFree{ noiseRunning= false };
            };
            noiseRunning= true;
        };
        // accent
        if(addAccent) {
            if((defCount%4)==0) { Synth.tail(ixxGroup, \ixx_accent) };
        };
        // update defcount
        this.changed(\defcount, defCount = defCount + 1);
        // return the Synth
        ^switch(fx)
        { 'clean' } {
            Synth.head(
                ixxGroup,
                defName,
                [
                    \out,    mainBus,
                    \env,    env,
                    \gate,   1,
                    \freq,   sequence.next.midicps,
                    \index,  indexBus.asMap,
                    \lpfreq, cfreqBus.asMap
                ]
            );
        }
        { 'gate' } {
            Synth.head(
                ixxGroup,
                defName,
                [
                    \out,    gateBus,
                    \env,    env,
                    \gate,   1,
                    \freq,   sequence.next.midicps,
                    \lpfreq, cfreqBus.asMap,
                    \index,  indexBus.asMap
                ]
            );
        }
        ;
    }

    masterVol {
        ^curVol;
    }

    masterVol_ {|volume|
        curVol = volume;
        this.changed(\mvol, curVol);
    }

    startTime {
        ^sTime.asTimeString.drop(-4).drop(3);
    }

    startTime_ {|time|
        sTime = 60*time;
    }

    speakerTest {|testIsRunning|
        if(testIsRunning) {
            ixxTest = Synth.head(mainGroup, \ixx_speakerTest, [\out,mainBus]);
        } {
            ixxTest.release;
        }
    }

    gui {
        ^ixxGui = ixxGui ?? { IXXGui(this) }
    }

    reset {
        noiseRunning  = false;
        addNoise[0..] = false;
        addAccent     = false;
        defCount      = 0;
        sequence.reset;
        noiseGroup.freeAll;
    }

    free {
        if(ixxGui.isClosed.not) { ixxGui.close };
        busses !? { busses.flat.do(_.free); busses= nil };
    }
}

IXXGui {

    var model, server;
    var sdict, specs, defName;
    var timeDisplay, countDisplay, cfreqDisplay, indexDisplay, mvolDisplay;
    var startTimeDisplay, speakerTestDisplay;
    var countKnob, cfreqKnob, indexKnob, mvolKnob, offsetBox;
    var btns, btnUpdate, paneClr;
    var counterRunning, time, count;
    var nextPage, prevPage;

    *new {|model|
        ^super.newCopyArgs(model).init;
    }

    init {
        server   = model.server;
        specs    = ControlSpec.specs;
        sdict    = (); // dictionary for storing synths
        paneClr  = Color.grey(0.11);
        time     = "-00:03";
        count    = "00";
        SimpleController(model)
        .put(\time, {|changer, what, t|
            var m,s;
            defer {
                m = (t.abs.div(60)%60).asString.padLeft(2, "0").add($:);
                s = (t.abs%60).asString.padLeft(2, "0");
                time = m ++ s;
                timeDisplay.refresh;
            }
        })
        .put(\defcount, {|changer, what, c|
            count = c.asString.padLeft(2, "0");
            countDisplay.refresh;
        })
        .put(\mvol, {|changer, what, value|
            mvolKnob.value_(value);
            model.mvolBus.set(value);
        });
        SimpleController(this)
        .put(\cfreq, {|changer, what, value|
            cfreqKnob.value_(value);
            model.cfreqBus.set(specs[\cfreq].map(value));
        })
        .put(\index, {|changer, what, value|
            indexKnob.value_(value);
            model.indexBus.set(specs[\index].map(value));
        });
        ^this.makeGui;
    }

    score {
        var w = Window().background_(paneClr),
        ub, width = w.bounds.width, height = w.bounds.height,
        wh = width/2, hh = height/2, drawGrid = true,
        rWidth = 20, // voices
        i = 0, j = 0, durations = model.durations, hDur, mDur, lDur;
        var instructions = List[
            "increase index/cutoff over 7 minutes",
            "(percussive events)",
            "start to introduce gated note events",
            "only play gated note events",
            "only play clean note events. rapidly decrease index/cutoff",
            "end"
        ].reverse;
        var font = Font("Monospaced",14);
        durations = durations.collect{|dur|
            Pseed(model.randSeed, Pn(Pshuf(dur))).asStream.nextN(200);
        };
        #hDur, mDur, lDur = durations;
        // note events
        ub = UserView(w,Rect(0,0,99999,height))
        .background_(Color.clear)
        .drawFunc_({
            var a=0!3, rh=40, g=3;
            var count    = 1!3;
            var timeClr  = Color.grey(0.1);
            var evClr    = Color.black;
            var vClr     = [ Color.green(0.5), Color.cyan(0.5), Color.yellow(0.5) ]; // voice colors
            var vOffsets = [ (hh/2)-(rh/2), hh-(rh/2), (hh+(hh/2))-(rh/2) ]; // vertical offsets
            [ hDur, mDur, lDur ].do{|time,i|
                time.do{|t|
                    var dur, r;
                    dur = t;
                    t = t*rWidth;
                    if(t>0) {
                        Pen.color = vClr[i];
                        Pen.fillRect(r = Rect(a[i]+(g/2), vOffsets[i], t-g, rh));
                        count[i].asString.padLeft(2,"0").drawInRect(r,font,evClr);
                        (dur.asString ++ '\"').drawRightJustIn(r.moveBy(-1,13),font,timeClr);
                        a[i] = a[i] + t;
                        count[i] = count[i] + 1;
                    } {
                        a[i] = a[i] + t.abs;
                    };
                };
            };
            // plot time code
            1001.do{|i|
                var x = 3*rWidth;
                var t = (0,3..3000);
                var rect;
                Pen.width = 0.5;
                (t[i].asTimeString.drop(-4).drop(3))
                .drawAtPoint((i*x)@(height-20),font,Color.white);
                // 0', 3', 4'30", 8', 8'45", 10'30"
                if(t[i]==0 or:{t[i]==180 or:{t[i]==270}
                    or:{t[i]==480} or:{t[i]==525} or:{t[i]==630}}) {
                    instructions.pop.drawAtPoint((i*x)@10,font,Color.white);
                }
            };
        });
        // grid
        if(drawGrid) {
            w.drawFunc = {
                var x = 3*rWidth; // smallest time value
                24.do {|i|
                    Pen.color = Color.grey(0.01);
                    Pen.width = 0.5;
                    Pen.line(x*(i+1)@0, x*(i+1)@height);
                };
                Pen.stroke;
            };
        };
        // 24 sec scroll (use multiples of 3 (eg. 3*8))
        nextPage = { i = i - (24*rWidth); ub.moveTo(i,0) };
        prevPage = { i = i + (24*rWidth); ub.moveTo(i,0) };
        w.view.fixedSize_(Size(1024,400));
        ^w.view;
    }

    makeGui {
        var w = Window("ixx").background_(Color.black),
        incStep = 0.0015,
        // cfreq
        curRout,      // temporary placeholder for routine
        cfreqRef = 0, // value to be incremented/decremented by routine
        cfreqIsRunning = false,
        cfreqRout = {|direction|
            Routine {
                cfreqIsRunning = true;
                inf.do{
                    cfreqRef = (cfreqRef+(direction*incStep)).clip(0,1);
                    this.changed(\cfreq, cfreqRef);
                    0.08.wait;
                }
            }
        },
        // index
        curIndexRout,  // temporary placeholder for routine
        indexRef  = 0, // value to be incremented/decremented by routine
        indexIsRunning = false,
        indexRout = {|direction|
            Routine {
                indexIsRunning = true;
                inf.do{
                    indexRef = (indexRef+(direction*incStep)).clip(0,1);
                    this.changed(\index, indexRef);
                    0.08.wait;
                }
            }
        },
        arrowKeys = Platform.case(
            \osx, { (left: 123, right: 124, up: 126, down: 125) },
            \linux, { (left: 113, right: 114, up: 111, down: 116) }
        ),
        resetFunc = {
            counterRunning !? { counterRunning.stop  };
            counterRunning !? { counterRunning.reset };
            counterRunning = nil;
            time  = "stop";
            count = "00";
            timeDisplay.refresh;
            countDisplay.refresh;
            model.reset; // reset to default
        };
        // add custom specs
        switch(model.register)
        { 'high' } {
            ControlSpec.specs[\cfreq]= [ 2500, 7500, \exp, 10, 2500, "Hz" ].asSpec;
            ControlSpec.specs[\index]= [ 0.5, 1.25, \lin, 0.01, 0.5 ].asSpec;
            model.cfreqBus.set(2500);
            model.indexBus.set(0.5);
        }
        { 'mid' } {
            ControlSpec.specs[\cfreq]= [ 500, 12500, \exp, 10, 500, "Hz" ].asSpec;
            ControlSpec.specs[\index]= [ 0.1, 0.9, \lin, 0.01, 0.1 ].asSpec;
            model.cfreqBus.set(500);
            model.indexBus.set(0.1);
        }
        { 'low' } {
            ControlSpec.specs[\cfreq]= [ 120, 12500, \exp, 10, 120, "Hz" ].asSpec;
            ControlSpec.specs[\index]= [ 0.0, 1.0, \lin, 0.01, 0.1 ].asSpec; // dummy values
            model.cfreqBus.set(120);
        };

        w.view.keyDownAction = {|... args|
            var view, char, mod, unicode, keycode, key;
            #view, char, mod, unicode, keycode, key = args;
            case
            { char==$z or:{char==$Z} } {
                sdict[char] ?? {
                    sdict[char]= model.createSynth('clean', 'long');
                }
            }
            { char==$x or:{char==$X} } {
                sdict[char] ?? {
                    sdict[char]= model.createSynth('clean', 'short');
                }
            }
            { char==$a or:{char==$A} } {
                sdict[char] ?? {
                    sdict[char]= model.createSynth('gate', 'long');
                }
            }
            { char==$s or:{char==$S} } {
                sdict[char] ?? {
                    sdict[char]= model.createSynth('gate', 'short');
                }
            }
            { char==$n or:{char==$N} } {
                nextPage.value;
            }
            { char==$p or:{char==$P} } {
                prevPage.value;
            }
            { keycode==arrowKeys[\right] } {
                if(cfreqIsRunning.not) {
                    curRout = cfreqRout.(1).reset.play(AppClock);
                }
            }
            { keycode==arrowKeys[\left] } {
                if(cfreqIsRunning.not) {
                    curRout = cfreqRout.(-1).reset.play(AppClock);
                }
            }
            // inc/dec index
            { keycode==arrowKeys[\up] } {
                if(indexIsRunning.not) {
                    curIndexRout = indexRout.(1).reset.play(AppClock);
                }
            }
            { keycode==arrowKeys[\down] } {
                if(indexIsRunning.not) {
                    curIndexRout = indexRout.(-1).reset.play(AppClock);
                }
            }
            // start/stop counter
            { unicode==13 } { // <CR>
                if(counterRunning.isNil) {
                    counterRunning = model.counter.start(SystemClock);
                } {
                    resetFunc.value;
                }
            }
            // hard-reset
            { unicode==27 } { // <ESC>
                resetFunc.value;
                time = "-00:03";
                offsetBox.valueAction_(0);
                cfreqKnob.valueAction_(0);
                indexKnob.valueAction_(0);
            }
            ;
        };
        w.view.keyUpAction = {|... args|
            var view, char, mod, unicode, keycode, key;
            #view, char, mod, unicode, keycode, key = args;
            case
            // inc/dec cfreq
            { keycode==arrowKeys[\right] } {
                if(cfreqIsRunning) {
                    curRout.stop;
                    cfreqIsRunning= false;
                    curRout = nil; // clean up
                }
            }
            { keycode==arrowKeys[\left] } {
                if(cfreqIsRunning) {
                    curRout.stop;
                    cfreqIsRunning= false;
                    curRout = nil; // clean up
                }
            }
            // inc/dec index
            { keycode==arrowKeys[\up] } {
                if(indexIsRunning) {
                    curIndexRout.stop;
                    indexIsRunning= false;
                    curIndexRout = nil; // clean up
                }
            }
            { keycode==arrowKeys[\down] } {
                if(indexIsRunning) {
                    curIndexRout.stop;
                    indexIsRunning= false;
                    curIndexRout = nil; // clean up
                }
            }
            // default
            {
                sdict[char] !? {
                    sdict[char].set(\gate, 0);
                    sdict[char]= nil;
                }
            }
            ;
        };

        // counter
        timeDisplay = UserView().drawFunc_({
            var font  = Font("Monospace",16);
            var color = Color.white;
            var rect  = Rect(0,0,100,20);
            "time:".drawInRect(rect.moveBy(2,2),font,color);
            time.drawInRect(rect.moveBy(55,2), font, color);
        }).fixedSize_(Size(100,40));
        // event count
        countDisplay = UserView().drawFunc_({
            var font  = Font("Monospace",16);
            var color = Color.white;
            var rect  = Rect(0,0,100,20);
            "event:".drawInRect(rect.moveBy(2,2), font, color);
            count.drawInRect(rect.moveBy(55,2), font, color);
        }).fixedSize_(Size(100,40));
        // start time offset
        startTimeDisplay = HLayout(
            StaticText().string_("offset"),
            offsetBox = NumberBox().action_({|nb|
                model.startTime_(nb.value)
            }).scroll_step_(0.1).keyDownAction_(false)
            .clipLo_(0).clipHi_(20)
            .normalColor_(Color.white).maxWidth_(50),
            [ StaticText().string_("min"),s:1 ]
        );
        // cfreq
        cfreqDisplay = VLayout(
            [
                cfreqKnob = Knob().action_(
                    {|kn|
                        var val  = kn.value;
                        cfreqRef = val;
                        this.changed(\cfreq, val);
                    }
                ).keyDownAction_(false),
            ],
            [ StaticText().string_("cutoff").align_(\center).maxSize_(Size(80,20)) ]
        );
        // index
        indexDisplay = VLayout(
            [
                indexKnob = Knob().action_(
                    {|kn|
                        var val  = kn.value;
                        indexRef = val;
                        this.changed(\index, val);
                    }
                ).keyDownAction_(false),
            ],
            [ StaticText().string_("index").align_(\center).maxSize_(Size(80,20)) ]
        );
        // master vol
        mvolDisplay = VLayout(
            [
                mvolKnob = Knob().value_(model.masterVol)
                    .action_(
                        {|sl|
                            var val = sl.value;
                            model.masterVol_(val);
                        }
                ).keyDownAction_(false),
            ],
            [ StaticText().string_("vol").align_(\center).maxSize_(Size(80,20)) ]
        );
        speakerTestDisplay = HLayout(
            [ nil,s:1 ],
            [ StaticText().string_("speaker test") ], [
                CheckBox().action_({|cb|
                    model.speakerTest(cb.value)
                })
            ],
            [ nil,s:1 ]
        );
        w.layout_(
            VLayout(
                this.score,
                HLayout(
                    View().background_(paneClr).fixedSize_(Size(250,100))
                    .layout_(
                        GridLayout()
                        .add(timeDisplay,0,0)
                        .add(startTimeDisplay,0,1)
                        .add(countDisplay,1,0)
                        .vSpacing_(10)
                        .margins_([ 15, 27 ])
                    ),
                    [
                        View().background_(paneClr)
                        .layout_(
                            if(model.register!='low') {
                                HLayout(
                                    mvolDisplay,
                                    indexDisplay,
                                    cfreqDisplay
                                )
                            } {
                                HLayout(
                                    mvolDisplay,
                                    cfreqDisplay
                                )
                            }
                        ),
                        s:1
                    ],
                    View().background_(paneClr).layout_(
                        VLayout(
                            StaticText().font_(Font("Monospace", 24))
                            .string_("(( IXX:"+model.register.asString+"))"),
                            HLayout(
                                [ nil,s:1 ],
                                [ StaticText().font_(Font("Monospace", 12))
                                    .string_("version: 1.1"),
                                    a:\c
                                ],
                                [ nil,s:1 ],
                            ),
                            speakerTestDisplay
                        )
                    )
                )
            )
        );
        w.onClose = {
            model.free;
            server.freeAll;
            counterRunning !? { counterRunning.stop };
            counterRunning = nil;
        };
        w.setTopLeftBounds(
            Rect(
                (Window.availableBounds.width/2)  - (w.bounds.width/2),
                (Window.availableBounds.height/2) - (w.bounds.height/2),
                w.bounds.width,
                w.bounds.height
            )
        );
        w.view.palette_(QPalette.dark);
        w.view.fixedSize_(Size(w.bounds.width, w.bounds.height));
        ^w.front;
    }
}
