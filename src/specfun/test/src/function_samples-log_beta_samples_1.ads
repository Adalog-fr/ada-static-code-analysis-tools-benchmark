package function_samples.log_beta_samples_1 is
Samples : constant Sample_Array :=
(
(5.0000000000000000e-01, 1.1447298858494002e+00),
(5.9999999999999998e-01, 1.0204712422537747e+00),
(6.9999999999999996e-01, 9.1860627945968254e-01),
(8.0000000000000004e-01, 8.3259943083239818e-01),
(9.0000000000000002e-01, 7.5835409683181432e-01),
(1.0000000000000000e+00, 6.9314718055994529e-01),
(1.1000000000000001e+00, 6.3508426736161605e-01),
(1.2000000000000002e+00, 5.8279855032844996e-01),
(1.3000000000000000e+00, 5.3527400633121180e-01),
(1.3999999999999999e+00, 4.9173630467541213e-01),
(1.5000000000000000e+00, 4.5158270528945488e-01),
(1.6000000000000001e+00, 4.1433543868345912e-01),
(1.7000000000000002e+00, 3.7960977872699536e-01),
(1.8000000000000000e+00, 3.4709161505069752e-01),
(1.9000000000000001e+00, 3.1652134455277492e-01),
(2.0000000000000000e+00, 2.8768207245178090e-01),
(2.1000000000000001e+00, 2.6039081792020546e-01),
(2.2000000000000002e+00, 2.3449185606023426e-01),
(2.2999999999999998e+00, 2.0985160589658380e-01),
(2.4000000000000004e+00, 1.8635465512423022e-01),
(2.5000000000000000e+00, 1.6390063283767398e-01),
(2.6000000000000001e+00, 1.4240172319981736e-01),
(2.7000000000000002e+00, 1.2178066942489552e-01),
(2.8000000000000003e+00, 1.0196915701771225e-01),
(2.9000000000000004e+00, 8.2906493371269763e-02),
(3.0000000000000000e+00, 6.4538521137571081e-02),
(3.1000000000000001e+00, 4.6816717622146564e-02),
(3.2000000000000002e+00, 2.9697443414221203e-02),
(3.3000000000000003e+00, 1.3141311650529675e-02),
(3.4000000000000004e+00, -2.8873445142982845e-03),
(3.5000000000000000e+00, -1.8420923956280699e-02),
(3.6000000000000001e+00, -3.3488943263846327e-02),
(3.7000000000000002e+00, -4.8118367370501680e-02),
(3.8000000000000003e+00, -6.2333894273564727e-02),
(3.9000000000000004e+00, -7.6158201258417435e-02),
(4.0000000000000000e+00, -8.9612158689687416e-02),
(4.0999999999999996e+00, -1.0271501634881730e-01),
(4.2000000000000002e+00, -1.1548456643027727e-01),
(4.3000000000000007e+00, -1.2793728660937598e-01),
(4.4000000000000004e+00, -1.4008846602778302e-01),
(4.5000000000000000e+00, -1.5195231658080344e-01),
(4.6000000000000005e+00, -1.6354207151204436e-01),
(4.7000000000000002e+00, -1.7487007300964530e-01),
(4.7999999999999998e+00, -1.8594785024074056e-01),
(4.9000000000000004e+00, -1.9678618904703260e-01),
(5.0000000000000000e+00, -2.0739519434607079e-01),
(5.1000000000000005e+00, -2.1778434613360487e-01),
(5.2000000000000002e+00, -2.2796254985696685e-01),
(5.3000000000000007e+00, -2.3793818182370430e-01),
(5.4000000000000004e+00, -2.4771913022014846e-01),
(5.5000000000000000e+00, -2.5731283223862889e-01),
(5.6000000000000005e+00, -2.6672630774727590e-01),
(5.7000000000000002e+00, -2.7596618988101440e-01),
(5.8000000000000007e+00, -2.8503875288497138e-01),
(5.9000000000000004e+00, -2.9394993750068021e-01),
(6.0000000000000000e+00, -3.0270537415039467e-01),
(6.1000000000000005e+00, -3.1131040414442701e-01),
(6.2000000000000002e+00, -3.1977009911008913e-01),
(6.3000000000000007e+00, -3.2808927881800187e-01),
(6.4000000000000004e+00, -3.3627252756159276e-01),
(6.5000000000000000e+00, -3.4432420922825990e-01),
(6.6000000000000005e+00, -3.5224848118543761e-01),
(6.7000000000000002e+00, -3.6004930709155580e-01),
(6.8000000000000007e+00, -3.6773046873008486e-01),
(6.9000000000000004e+00, -3.7529557695463289e-01),
(7.0000000000000000e+00, -3.8274808182393194e-01),
(7.1000000000000005e+00, -3.9009128199754084e-01),
(7.2000000000000002e+00, -3.9732833345596497e-01),
(7.3000000000000007e+00, -4.0446225760257537e-01),
(7.4000000000000004e+00, -4.1149594879918094e-01),
(7.5000000000000000e+00, -4.1843218138197891e-01),
(7.6000000000000005e+00, -4.2527361620032877e-01),
(7.7000000000000002e+00, -4.3202280671664361e-01),
(7.8000000000000007e+00, -4.3868220470237063e-01),
(7.9000000000000004e+00, -4.4525416556154163e-01),
(8.0000000000000000e+00, -4.5174095331088360e-01),
(8.1000000000000014e+00, -4.5814474524255999e-01),
(8.1999999999999993e+00, -4.6446763629358934e-01),
(8.3000000000000007e+00, -4.7071164314377612e-01),
(8.4000000000000004e+00, -4.7687870806203136e-01),
(8.5000000000000000e+00, -4.8297070251955354e-01),
(8.5999999999999996e+00, -4.8898943058643418e-01),
(8.7000000000000011e+00, -4.9493663212721017e-01),
(8.8000000000000007e+00, -5.0081398580937453e-01),
(8.9000000000000004e+00, -5.0662311193783616e-01),
(9.0000000000000000e+00, -5.1236557512731729e-01),
(9.0999999999999996e+00, -5.1804288682362909e-01),
(9.2000000000000011e+00, -5.2365650768392591e-01),
(9.3000000000000007e+00, -5.2920784982538471e-01),
(9.4000000000000004e+00, -5.3469827895085942e-01),
(9.5000000000000000e+00, -5.4012911635950189e-01),
(9.5999999999999996e+00, -5.4550164084977482e-01),
(9.7000000000000011e+00, -5.5081709052167227e-01),
(9.8000000000000007e+00, -5.5607666448442039e-01),
(9.9000000000000004e+00, -5.6128152447569768e-01),
(1.0000000000000000e+01, -5.6643279639759037e-01),
(1.0100000000000001e+01, -5.7153157177461189e-01),
(1.0200000000000001e+01, -5.7657890913826471e-01),
(1.0300000000000001e+01, -5.8157583534270252e-01),
(1.0400000000000000e+01, -5.8652334681544538e-01),
(1.0500000000000000e+01, -5.9142241074705559e-01),
(1.0600000000000001e+01, -5.9627396622320106e-01),
(1.0700000000000001e+01, -6.0107892530255924e-01),
(1.0800000000000001e+01, -6.0583817404349105e-01),
(1.0900000000000000e+01, -6.1055257348248126e-01),
(1.1000000000000000e+01, -6.1522296056702075e-01),
(1.1100000000000001e+01, -6.1985014904542268e-01),
(1.1200000000000001e+01, -6.2443493031589625e-01),
(1.1300000000000001e+01, -6.2897807423728480e-01),
(1.1400000000000000e+01, -6.3348032990321457e-01),
(1.1500000000000000e+01, -6.3794242638194731e-01),
(1.1600000000000001e+01, -6.4236507342346272e-01),
(1.1700000000000001e+01, -6.4674896213574939e-01),
(1.1800000000000001e+01, -6.5109476563161195e-01),
(1.1900000000000000e+01, -6.5540313964783437e-01),
(1.2000000000000000e+01, -6.5967472313786146e-01),
(1.2100000000000001e+01, -6.6391013883945860e-01),
(1.2200000000000001e+01, -6.6810999381856462e-01),
(1.2300000000000001e+01, -6.7227487999060642e-01),
(1.2400000000000000e+01, -6.7640537462025208e-01),
(1.2500000000000000e+01, -6.8050204080073939e-01),
(1.2600000000000001e+01, -6.8456542791384223e-01),
(1.2700000000000001e+01, -6.8859607207124185e-01),
(1.2800000000000001e+01, -6.9259449653836569e-01),
(1.2900000000000000e+01, -6.9656121214133648e-01),
(1.3000000000000000e+01, -7.0049671765811183e-01),
(1.3100000000000001e+01, -7.0440150019418368e-01),
(1.3200000000000001e+01, -7.0827603554389995e-01),
(1.3300000000000001e+01, -7.1212078853780980e-01),
(1.3400000000000000e+01, -7.1593621337688518e-01),
(1.3500000000000000e+01, -7.1972275395402008e-01),
(1.3600000000000001e+01, -7.2348084416351455e-01),
(1.3700000000000001e+01, -7.2721090819902656e-01),
(1.3800000000000001e+01, -7.3091336084049274e-01),
(1.3900000000000000e+01, -7.3458860773057921e-01),
(1.4000000000000000e+01, -7.3823704564096460e-01),
(1.4100000000000001e+01, -7.4185906272909463e-01),
(1.4200000000000001e+01, -7.4545503878565356e-01),
(1.4300000000000001e+01, -7.4902534547326027e-01),
(1.4400000000000000e+01, -7.5257034655666288e-01),
(1.4500000000000000e+01, -7.5609039812489343e-01),
(1.4600000000000001e+01, -7.5958584880563151e-01),
(1.4700000000000001e+01, -7.6305703997216057e-01),
(1.4800000000000001e+01, -7.6650430594320440e-01),
(1.4900000000000000e+01, -7.6992797417589642e-01),
(1.5000000000000000e+01, -7.7332836545223316e-01),
(1.5100000000000001e+01, -7.7670579405926077e-01),
(1.5200000000000001e+01, -7.8006056796312606e-01),
(1.5300000000000001e+01, -7.8339298897746446e-01),
(1.5400000000000000e+01, -7.8670335292612492e-01),
(1.5500000000000000e+01, -7.8999194980057652e-01),
(1.5600000000000001e+01, -7.9325906391221324e-01),
(1.5700000000000001e+01, -7.9650497403971343e-01),
(1.5800000000000001e+01, -7.9972995357152854e-01),
(1.5900000000000000e+01, -8.0293427064405165e-01),
(1.6000000000000000e+01, -8.0611818827522086e-01),
(1.6100000000000001e+01, -8.0928196449387357e-01),
(1.6200000000000003e+01, -8.1242585246515375e-01),
(1.6300000000000001e+01, -8.1555010061199340e-01),
(1.6399999999999999e+01, -8.1865495273272160e-01),
(1.6500000000000000e+01, -8.2174064811516345e-01),
(1.6600000000000001e+01, -8.2480742164714727e-01),
(1.6699999999999999e+01, -8.2785550392377516e-01),
(1.6800000000000001e+01, -8.3088512135131865e-01),
(1.6900000000000002e+01, -8.3389649624802686e-01),
(1.7000000000000000e+01, -8.3688984694197543e-01),
(1.7100000000000001e+01, -8.3986538786595233e-01),
(1.7199999999999999e+01, -8.4282332964953355e-01),
(1.7300000000000001e+01, -8.4576387920849072e-01),
(1.7400000000000002e+01, -8.4868723983159811e-01),
(1.7500000000000000e+01, -8.5159361126483901e-01),
(1.7600000000000001e+01, -8.5448318979326388e-01),
(1.7699999999999999e+01, -8.5735616832047867e-01),
(1.7800000000000001e+01, -8.6021273644584539e-01),
(1.7900000000000002e+01, -8.6305308053947982e-01),
(1.8000000000000000e+01, -8.6587738381522428e-01),
(1.8100000000000001e+01, -8.6868582640144609e-01),
(1.8199999999999999e+01, -8.7147858540991052e-01),
(1.8300000000000001e+01, -8.7425583500279913e-01),
(1.8400000000000002e+01, -8.7701774645783104e-01),
(1.8500000000000000e+01, -8.7976448823153675e-01),
(1.8600000000000001e+01, -8.8249622602093325e-01),
(1.8699999999999999e+01, -8.8521312282344411e-01),
(1.8800000000000001e+01, -8.8791533899517106e-01),
(1.8900000000000002e+01, -8.9060303230771609e-01),
(1.9000000000000000e+01, -8.9327635800334093e-01),
(1.9100000000000001e+01, -8.9593546884881192e-01),
(1.9199999999999999e+01, -8.9858051518768889e-01),
(1.9300000000000001e+01, -9.0121164499132789e-01),
(1.9400000000000002e+01, -9.0382900390848420e-01),
(1.9500000000000000e+01, -9.0643273531370028e-01),
(1.9600000000000001e+01, -9.0902298035437212e-01),
(1.9700000000000003e+01, -9.1159987799663611e-01),
(1.9800000000000001e+01, -9.1416356507011187e-01),
(1.9900000000000002e+01, -9.1671417631138752e-01),
(2.0000000000000000e+01, -9.1925184440660246e-01)
);

Extra : constant Float_Type :=5.0000000000000000e-01;
end function_samples.log_beta_samples_1;