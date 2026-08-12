# Review of Detecting departure and destination aerodromes from ADS-B 
## Version 6 - The consolidates study, and what to run in production

## General remarks

1. The paper is very complete. You detail the sweep harness and then validate the results in production. This is good, however, you also detail several of the things that went wrong and that you fixed. This is not really necessary, I just want to see the final results in the paper, not the wrong turns or bugfixes that were made.
2. You are writing this paper for external readers, not for the person orchestrating the development. Hence it's good to be clear about each term used and when you show a table to explain each of the headers (where not fully clear).
3. You use production and legacy interchangably it seems.

## Comments on Summary
You mention a mistake was made (which, as stated above, is not relevant for the end reader). It is however relevant that a more efficient algorithm was developed.

## Comments on 1. How the metrics work

I like the metric introduction. It's however not clear that in table 1, you suddenly introduce recall for OOA only and then later in the paper (last few chapters) you reuse the term recall without reintroducing it and reframing it in the new context. 

## Comments on 3. The three methods

In 3.3 I noted down that you apply a scheduled-service penalty tiebreak. I noted down here: Does it not make sense to also apply this in trend? Because in this point in the paper it hasn't been mentioned for trend. Later in the paper it does seem you do apply it for trend as well, is this correct? If not, we should try that out and test it. 

## Comments on 4. Trend: The full parameter sweep
Important comment: You should probably apply any parameter sweep (both in the harness sweep and in production) on both time periods. We are trying to establish the best method for both periods, so always apply the tests on both periods simultaneously and see what works for all. 

You should probably introduce somewhere here that we will do both a harness sweep (i.e., research) which we will later (reference the chapter) apply in the production pipeline. Currently you just start doing the sweep and explaining it but the transition to the production pipeline is abrupt and unclear. It seems here you are already applying things in the production pipeline. 

Also give context on which data you are applying this, write a proper introduction on which day you apply this. 

## Comments on 8. Negative results, re-measured

Here this section makes sense for the developer, but to a random reader this is not properly introduced I find. Introduce this better in the whole paper. 

Also in 8.3 Bearing applied to trend, you mention that the tie-break by allignment within 2NM improves the shipped configuration by 1140. What do you mean with "within 2NM" how does this work? This is not explained in the paper. Also, if it is such a big improvement to trend, why do we not apply this in production? 

In the section 8.5 The sampler: bin-based against fixed-phase; we should probably not put this under negative results but introduce this as the default improvement of the pipeline later on. It's something which will improve things down the line (not necessarily for ADEP/ADES detection but for example to extract flight events such as inblocks, take-off, ... on ground as we will have more statevectors.

In the table in 8.5 you mention "Cells" but this is never introduced. Could you explain this at least? Also I noted down that it would be better to see improvement on the selected parameters for the model in addition (2nd table) as currently you just show median score (which is also not explained). Median score of what? 

## Comments on 9. What the pipeline actually produces

Here is the first time you introduce running things in the production pipeline, this introduction is too late and should explain that whilst the code worked well in the research setting we are now going to test things in production and see how they evolve. Note again that it should be readable not only for the developer of this product who's instructing claude but also for the general public who will read this. 

You mention three points; again this is things that went wrong (counting by H3 ring count was never intended to be applied in production) we want to implement our research into production, not use the legacy production methods. We wanted haversine to be used. We want to keep the smoothed barometric altitude across the flight-level boundary. 

Also, the OPDI production pipeline has ingestion, statevector processing (filters etc), ... which is never explained here. I do hope it is applied in your production pipeline test? Please confirm if not.. We should include the steps that are done in the pipeline, maybe you can even produce a flow chart with step by step descriptions. 

You say the "endpoint reproduces exactly" but that this is not luck: the endpoint sweep filters the same cached candidate table. I noted on this the following: I hope this cached candidate table is now produced by the pipeline right? This should not be a remnant from an older run but be part of the OPDI processing pipeline. 

You say "trend does not. The harness ranks candidate aerodromes by exact haversine distance. The pipeline first keeps only the candidates at the minimum H3 ring count." >> What do you mean with this? Why filter on minimum ring count. Just match the full H3 disk we created (at the selected NM for the test) to find candidates and then calc haversine to tie break. Isn't that what the sweep harness does as well? 

## Comments on 9.1 What running the pipeline caught

You should only show numbers where all bugs are fixed. No need to discuss the bugs, we're writing this paper for an external reader, not the developer. No need to discuss your progress steps and things you fixed. You can report that to me in the chat, but no need to put it in the paper. 

## Comments on 9.2 The trend tuning re-walked inside the pipeline

Is the exact distance ranking still using a minimum H3 ring count or does it match the full disk (up to the selected radius) and then apply haversine? 

In the table under 9.2 you add coverage + accuracy, but for which mode? I don't understand. It's not explained. 

## Comment on 9.3 Production's own optimum

Here you discuss ADES and ADEP sweeping in production, but you do not discuss which modes you're using or which parameters you are applying really? 

You mention the vote margin, this is only for trend right? 

## Comment on 10.1 Which change is actually doing the work

The table is not clear, what are you showing here in each step/row? Explain it simpler / better. 

## Comment on 11

Same for the table here; what are you showing here in each row? 

## In 11.1 What is being recommended and why

You mention that for departures the flight level cap should be 15000 ft or FL150, but earlier you said 120 was optimal no? Why did it change. Should we do the sweep up to FL250 or FL300? It seems to be getting better the higher we go. 

## in 13. The busiest 100 aerodromes

Here is the recall you introduce but never explain properly. I wonder here if recall here means you macht flight by flight on icao24 or similar? Is that what you mean or do you just compare movement numbers somehow? Do a proper explanation. Also in the table you have recall and recall production. What's what here? Why is production always lower? Is this the original recall? I don't get it. What's the new method implemented and the legacy? 

## In 14. Does any of it hold on a second period

In the table for trend it's clear it's not. We should train and finetune the model on both periods and then selected the optimal parameters for both. We can then validate for a third period in between I guess. 

## In 17. Limitations

You mention trend emits no out of area label at all, why is there no out of area for trend. Should we implement it? I think we should. 