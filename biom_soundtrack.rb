use_bpm 60

live_loop :stomp do
  sample :bd_haus, amp: 4
  sleep 1
end

live_loop :samples do
  sample :loop_compus, beat_stretch: 8, amp: 2
  sleep 8
end

live_loop :synthsound do
  use_synth :subpulse
  4.times do
    play_pattern_timed [:c4, :e4], [0.25, 0.25], sustain:0
  end
  4.times do
    play_pattern_timed [:c4, :f4], [0.25, 0.25], sustain:0
  end
  4.times do
    play_pattern_timed [:c4, :e4], [0.25, 0.25], sustain:0
  end
  4.times do
    play_pattern_timed [:c4, :d4], [0.25, 0.25], sustain:0
  end
end


