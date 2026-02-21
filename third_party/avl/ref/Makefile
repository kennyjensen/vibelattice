FC ?= gfortran
FFLAGS ?= -O2 -std=legacy

AVL_SRC_DIR := ../src

REF_BINS := m3inv_ref rateki3_ref rotens3_ref cdcl_ref vorvel_ref vorvelc_ref matrix_ref matrix_ref_dp lapacksubs_s_ref lapacksubs_d_ref matrix_linpack_ref avl_ref asetup_ref ba_trans_ref aic_ref atpforc_ref bdforc_ref aero_ref spline_ref sgutil_ref airutil_ref limits_ref limits2_ref amass_ref atrim_ref exec_ref aoutmrf_ref amode_ref plane_exec_ref b737_exec_ref supra_exec_ref
REF_SRC := m3inv_ref.f rateki3_ref.f rotens3_ref.f cdcl_ref.f vorvel_ref.f vorvelc_ref.f matrix_ref.f90 matrix_ref_dp.f90 lapacksubs_s_ref.f90 lapacksubs_d_ref.f90 matrix_linpack_ref.f avl_deps.f asetup_ref.f ba_trans_ref.f aic_ref.f atpforc_ref.f bdforc_ref.f aero_ref.f spline_ref.f sgutil_ref.f airutil_ref.f limits_ref.f limits2_ref.f limits_deps.f amass_ref.f amass_deps.f atrim_ref.f atrim_deps.f exec_ref.f aoutmrf_ref.f amode_ref.f plane_exec_ref.f b737_exec_ref.f supra_exec_ref.f plane_exec_stubs.f
AVL_SRC := $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/vorvel.f $(AVL_SRC_DIR)/vorvelc.f $(AVL_SRC_DIR)/matrix.f $(AVL_SRC_DIR)/ba_trans.f $(AVL_SRC_DIR)/aic.f

all: $(REF_BINS)

%_ref: %_ref.f $(AVL_SRC)
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC)

vorvel_ref: vorvel_ref.f $(AVL_SRC_DIR)/vorvel.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/vorvel.f

vorvelc_ref: vorvelc_ref.f $(AVL_SRC_DIR)/vorvelc.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/vorvelc.f

matrix_ref: matrix_ref.f90 $(AVL_SRC)
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC)

matrix_ref_dp: matrix_ref_dp.f90
	$(FC) $(FFLAGS) -o $@ $<

matrix_linpack_ref: matrix_linpack_ref.f $(AVL_SRC_DIR)/matrix-linpack.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/matrix-linpack.f

avl_ref: avl_deps.f $(AVL_SRC_DIR)/avl.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ $(AVL_SRC_DIR)/avl.f avl_deps.f

avl_heap_inc.o: $(AVL_SRC_DIR)/avl_heap_inc.f90
	$(FC) $(FFLAGS) -c $< -o $@

asetup_ref: asetup_ref.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/matrix.f avl_heap_inc.o
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ asetup_ref.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/matrix.f avl_heap_inc.o

exec_ref: exec_ref.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f avl_heap_inc.o
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ exec_ref.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f avl_heap_inc.o

lapacksubs_s_ref: lapacksubs_s_ref.f90 $(AVL_SRC_DIR)/matrix-lapacksubs.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/matrix-lapacksubs.f

lapacksubs_d_ref: lapacksubs_d_ref.f90 $(AVL_SRC_DIR)/matrix-lapacksubs.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/matrix-lapacksubs.f

aic_ref: aic_ref.f $(AVL_SRC_DIR)/aic.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/aic.f

atpforc_ref: atpforc_ref.f $(AVL_SRC_DIR)/atpforc.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ $< $(AVL_SRC_DIR)/atpforc.f

bdforc_ref: bdforc_ref.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/aic.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ $< $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/aic.f

aero_ref: aero_ref.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/cdcl.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ $< $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/cdcl.f

spline_ref: spline_ref.f $(AVL_SRC_DIR)/spline.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/spline.f

sgutil_ref: sgutil_ref.f $(AVL_SRC_DIR)/sgutil.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/sgutil.f

airutil_ref: airutil_ref.f $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/userio.f
	$(FC) $(FFLAGS) -o $@ $< $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/userio.f

limits_ref: limits_ref.f limits_deps.f $(AVL_SRC_DIR)/limits.f $(AVL_SRC_DIR)/autil.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ limits_ref.f limits_deps.f $(AVL_SRC_DIR)/limits.f $(AVL_SRC_DIR)/autil.f

limits2_ref: limits2_ref.f limits_deps.f $(AVL_SRC_DIR)/limits2.f $(AVL_SRC_DIR)/autil.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ limits2_ref.f limits_deps.f $(AVL_SRC_DIR)/limits2.f $(AVL_SRC_DIR)/autil.f

amass_ref: amass_ref.f amass_deps.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/userio.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ amass_ref.f amass_deps.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/userio.f

atrim_ref: atrim_ref.f atrim_deps.f $(AVL_SRC_DIR)/atrim.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ atrim_ref.f atrim_deps.f $(AVL_SRC_DIR)/atrim.f

aoutmrf_ref: aoutmrf_ref.f $(AVL_SRC_DIR)/aoutmrf.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ aoutmrf_ref.f $(AVL_SRC_DIR)/aoutmrf.f

amode_ref: amode_ref.f $(AVL_SRC_DIR)/autil.f
	$(FC) $(FFLAGS) -I$(AVL_SRC_DIR) -o $@ amode_ref.f $(AVL_SRC_DIR)/autil.f

plane_exec_ref: plane_exec_ref.f plane_exec_stubs.f $(AVL_SRC_DIR)/ainput.f $(AVL_SRC_DIR)/amake.f $(AVL_SRC_DIR)/aoper.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/userio.f $(AVL_SRC_DIR)/sgutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/vorvel.f $(AVL_SRC_DIR)/ba_trans.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/matrix.f $(AVL_SRC_DIR)/avl_heap.f90 avl_heap_inc.o
	$(FC) $(FFLAGS) -fdefault-real-8 -I$(AVL_SRC_DIR) -o $@ plane_exec_ref.f plane_exec_stubs.f $(AVL_SRC_DIR)/ainput.f $(AVL_SRC_DIR)/amake.f $(AVL_SRC_DIR)/aoper.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/userio.f $(AVL_SRC_DIR)/sgutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/vorvel.f $(AVL_SRC_DIR)/ba_trans.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/matrix.f $(AVL_SRC_DIR)/avl_heap.f90 avl_heap_inc.o

b737_exec_ref: b737_exec_ref.f plane_exec_stubs.f $(AVL_SRC_DIR)/ainput.f $(AVL_SRC_DIR)/amake.f $(AVL_SRC_DIR)/aoper.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/userio.f $(AVL_SRC_DIR)/sgutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/vorvel.f $(AVL_SRC_DIR)/ba_trans.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/matrix.f $(AVL_SRC_DIR)/avl_heap.f90 avl_heap_inc.o
	$(FC) $(FFLAGS) -fdefault-real-8 -I$(AVL_SRC_DIR) -o $@ b737_exec_ref.f plane_exec_stubs.f $(AVL_SRC_DIR)/ainput.f $(AVL_SRC_DIR)/amake.f $(AVL_SRC_DIR)/aoper.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/userio.f $(AVL_SRC_DIR)/sgutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/vorvel.f $(AVL_SRC_DIR)/ba_trans.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/matrix.f $(AVL_SRC_DIR)/avl_heap.f90 avl_heap_inc.o

supra_exec_ref: supra_exec_ref.f plane_exec_stubs.f $(AVL_SRC_DIR)/ainput.f $(AVL_SRC_DIR)/amake.f $(AVL_SRC_DIR)/aoper.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/userio.f $(AVL_SRC_DIR)/sgutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/vorvel.f $(AVL_SRC_DIR)/ba_trans.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/matrix.f $(AVL_SRC_DIR)/avl_heap.f90 avl_heap_inc.o
	$(FC) $(FFLAGS) -fdefault-real-8 -I$(AVL_SRC_DIR) -o $@ supra_exec_ref.f plane_exec_stubs.f $(AVL_SRC_DIR)/ainput.f $(AVL_SRC_DIR)/amake.f $(AVL_SRC_DIR)/aoper.f $(AVL_SRC_DIR)/asetup.f $(AVL_SRC_DIR)/aic.f $(AVL_SRC_DIR)/aero.f $(AVL_SRC_DIR)/atpforc.f $(AVL_SRC_DIR)/cdcl.f $(AVL_SRC_DIR)/autil.f $(AVL_SRC_DIR)/userio.f $(AVL_SRC_DIR)/sgutil.f $(AVL_SRC_DIR)/spline.f $(AVL_SRC_DIR)/airutil.f $(AVL_SRC_DIR)/vorvel.f $(AVL_SRC_DIR)/ba_trans.f $(AVL_SRC_DIR)/amass.f $(AVL_SRC_DIR)/matrix.f $(AVL_SRC_DIR)/avl_heap.f90 avl_heap_inc.o

clean:
	rm -f $(REF_BINS)

.PHONY: all clean
