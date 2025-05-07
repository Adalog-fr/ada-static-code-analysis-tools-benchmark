-- Configuration options for TLS contexts
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_95;

package body TLS.Configure is

	------------------
	-- Enable_TLSv1 --
	------------------

	procedure Enable_TLSv1 (Conf : in out Config) is
	begin
		Conf.TLSv1_0 := True;
		Conf.TLSv1_1 := True;
		Conf.TLSv1_2 := True;
		Conf.TLSv1_3 := True;
	end Enable_TLSv1;


	--------------------
	-- Enable_All_TLS --
	--------------------

	procedure Enable_All_TLS (Conf : in out Config) is
	begin
		Enable_TLSv1(Conf);
	end Enable_All_TLS;


	-----------------------
	-- Enable_Secure_TLS --
	-----------------------

	procedure Enable_Secure_TLS (Conf : in out Config) is
	begin
		Conf.TLSv1_2 := True;
		Conf.TLSv1_3 := True;
		Conf.TLSv1_0 := False;
		Conf.TLSv1_1 := False;
	end Enable_Secure_TLS;

end TLS.Configure;
