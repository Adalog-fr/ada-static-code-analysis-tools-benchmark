
if 0 
  betaln_q = 0.5:0.1:20;
  betaln_p = [0.5, 3.0, 7.4, 12.1, 20.6];

  crea_sample_array ({betanl_p, betanl_q},  ...
		     @betaln, ...
		     'src/',  ...
		     'function_samples.log_beta_samples');
end

if 1
  betainc_x = 0:0.05:0.9;
  betainc_a = [0.5, 3.0, 7.4, 12.1, 20.6];
  betainc_b = 0.1:0.1:20; %0.5:0.1:20;

  crea_sample_array({betainc_x, betainc_a, betainc_b}, ...
		    @betainc, ...
		    'src/',   ...
		    'function_samples.beta_inc_samples', ...
		    'Triplet_Test.');
end
