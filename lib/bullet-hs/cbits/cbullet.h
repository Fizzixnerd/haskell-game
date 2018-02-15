#ifndef CBULLET_H
#define CBULLET_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

  // poor man's newtype.  Dear God.
  typedef struct action_interface { char unused; } action_interface;
  typedef struct box_shape { char unused; } box_shape;
  typedef struct broadphase_interface { char unused; } broadphase_interface;
  typedef struct capsule_shape { char unused; } capsule_shape;
  typedef struct collision_configuration { char unused; } collision_configuration;
  typedef struct collision_dispatcher { char unused; } collision_dispatcher;
  typedef struct collision_object { char unused; } collision_object;
  typedef struct collision_shape { char unused; } collision_shape;
  typedef struct constraint_solver { char unused; } constraint_solver;
  typedef struct convex_shape { char unused; } convex_shape;
  typedef struct dynamics_world { char unused; } dynamics_world;
  typedef struct kinematic_character_controller { char unused; }
    kinematic_character_controller;
  typedef struct motion_state { char unused; } motion_state;
  typedef struct pair_caching_ghost_object { char unused; }
    pair_caching_ghost_object;
  typedef struct rigid_body { char unused; } rigid_body;
  typedef struct rigid_body_construction_info { char unused; }
    rigid_body_construction_info;
  typedef struct serializer { char unused; } serializer;
  typedef struct sphere_shape { char unused; } sphere_shape;
  typedef struct static_plane_shape { char unused; } static_plane_shape;
  typedef struct transform { char unused; } transform;
  typedef struct typed_constraint { char unused; } typed_constraint;
  typedef struct overlapping_pair_cache { char unused; } overlapping_pair_cache;
  typedef struct ghost_pair_callback { char unused; } ghost_pair_callback;
  typedef float scalar;
  typedef dynamics_world collision_world;

  // btDbvtBroadphase
  broadphase_interface* new_broadphase_interface();
  void free_broadphase_interface(broadphase_interface* broadphase);
  overlapping_pair_cache* get_overlapping_pair_cache
  (broadphase_interface* broadphase);

  // btOverlappingPairCache
  void set_internal_ghost_pair_callback(overlapping_pair_cache* cache,
					ghost_pair_callback* callback);

  // btGhostPairCallback
  ghost_pair_callback* new_ghost_pair_callback();
  void free_ghost_pair_callback(ghost_pair_callback* callback);

  // btDefaultCollisionConfiguration
  collision_configuration* new_default_collision_configuration();
  void free_collision_configuration(collision_configuration* collision_configuration);

  collision_dispatcher* new_collision_dispatcher(collision_configuration* collision_configuration);
  void free_collision_dispatcher(collision_dispatcher* collision_dispatcher);

  constraint_solver* new_sequential_impulse_constraint_solver();
  void free_constraint_solver(constraint_solver* constraint_solver);

  // btDiscreteDynamicsWorld
  dynamics_world* new_discrete_dynamics_world (collision_dispatcher* dispatcher, 
					       broadphase_interface* broadphase, 
					       constraint_solver* solver, 
					       collision_configuration* collision_configuration);
  void free_dynamics_world(dynamics_world* world);
  void dw_set_gravity(dynamics_world* world, scalar x, scalar y, scalar z);
  int step_simulation(dynamics_world* world,
		      scalar time_step,
		      const int* max_sub_steps,
		      const scalar* fixed_time_step);
  //  int default_step_simulation(discrete_dynamics_world* world, scalar time_step);
  void add_rigid_body(dynamics_world* world, rigid_body* rigid_body);
  int get_num_collision_objects(const dynamics_world* world);
  collision_object* get_collision_object(const dynamics_world* world, int idx);
  void add_collision_object(dynamics_world* world, collision_object* obj);
  void remove_collision_object(dynamics_world* world, collision_object* obj);
  void add_action(dynamics_world* world, action_interface* action);
  void remove_action(dynamics_world* world, action_interface* action);
  int get_num_constraints(const dynamics_world* world);
  typed_constraint* get_constraint(const dynamics_world* world, int idx);
  void add_constraint(dynamics_world* world, typed_constraint* constriant);
  void remove_constraint(dynamics_world* world, typed_constraint* constraint);
  void dw_serialize(const dynamics_world* world, serializer* serializer);

  //btCollisionObject
  rigid_body* collision_object_to_rigid_body(const collision_object* obj);
  transform* co_allocate_world_transform(const collision_object* obj);
  int isStaticObject(const collision_object* obj);
  int isKinematicObject(const collision_object* obj);
  int hasContactResponse(const collision_object* obj);

  // btDefaultMotionState
  motion_state* new_default_motion_state(transform* transform);
  void free_motion_state(motion_state* motion_state);
  transform* ms_allocate_world_transform(const motion_state* motion_state);

  // byRigidBodyContructionInfo
  rigid_body_construction_info* new_rigid_body_construction_info
  (scalar mass,
   motion_state* motion_state,
   collision_shape* collision_shape,
   scalar inertia_x,
   scalar inertia_y,
   scalar inertia_z);
  void free_rigid_body_construction_info(rigid_body_construction_info* rbci);

  // btRigidBody
  rigid_body* new_rigid_body(rigid_body_construction_info* rbci);
  void free_rigid_body(rigid_body* rigid_body);
  motion_state* rb_get_motion_state(const rigid_body* rigid_body);
  //  int is_static_object(const rigid_body* rigid_body); // returns bool
  //  int is_kinematic_object(const rigid_body* rigid_body); // returns bool
  void set_activation_state(rigid_body* rigid_body, int new_state);
  transform* allocate_center_of_mass_transform(rigid_body* rigid_body);

  // btCollisionShape
  void calculate_local_inertia(collision_shape* collision_shape,
			       scalar mass,
			       scalar* x_out,
			       scalar* y_out,
			       scalar* z_out);
  void free_collision_shape(collision_shape* collision_shape);

  // btStaticPlaneShape
  static_plane_shape* new_static_plane_shape(scalar x,
					     scalar y,
					     scalar z,
					     scalar plane_constant);
  void free_static_plane_shape(static_plane_shape* static_plane_shape);

  // btCapsuleShape
  capsule_shape* new_capsule_shape(scalar radius, scalar height);
  void free_capsule_shape(capsule_shape* capsule_shape);
  convex_shape* capsule_shape_to_convex_shape(capsule_shape* capsule_shape);

  // btSphereShape
  sphere_shape* new_sphere_shape(scalar radius);
  void free_sphere_shape(sphere_shape* sphere_shape);
  convex_shape* sphere_shape_to_convex_shape(sphere_shape* sphere_shape);

  // btBoxShape
  box_shape* new_box_shape(scalar half_x, scalar half_y, scalar half_z);
  void free_box_shape(box_shape* box_shape);
  convex_shape* box_shape_to_convex_shape(box_shape* box_shape);

  // btTransform
  transform* new_transform(scalar i, // quaternion
			   scalar j,
			   scalar k,
			   scalar r,
			   scalar x, // vector
			   scalar y,
			   scalar z);
  void free_transform(transform* transform);
  void get_origin(const transform* transform, scalar* x_out, scalar* y_out, scalar* z_out);
  void set_origin(transform* transform, scalar x, scalar y, scalar z);
  void get_rotation(const transform* transform, scalar* i_out, scalar* j_out,
		    scalar* k_out, scalar* r_out);
  void set_rotation(transform* transform, scalar i, scalar j, scalar k, scalar r);
  void set_identity(transform* transform);
  void get_opengl_matrix(const transform* transform, scalar* out /* size 16 */);

  // btTypedConstraint
  void free_typed_constraint(typed_constraint* constraint);

  // btPoint2PointConstraint
  typed_constraint* new_point2point_constraint(rigid_body* rigid_body,
					       scalar pivot_x,
					       scalar pivot_y,
					       scalar pivot_z);

  // btDefaultSerializer
  serializer* new_default_serializer();
  void free_serializer(serializer* serializer);
  const unsigned char* get_buffer_pointer(serializer* serializer);
  int get_current_buffer_size(const serializer* serializer);

  // btPairCachingGhostObject 
  // TODO: Finish API
  pair_caching_ghost_object* new_pair_caching_ghost_object();
  void free_pair_caching_ghost_object(pair_caching_ghost_object* ghost_object);
  //collision_object* pair_caching_ghost_object_to_collision_object
  //(const pair_caching_ghost_object* ghost_object);
  //void pcgo_set_world_transform(pair_caching_ghost_object* ghost_object,
  //				transform* transform);
  //void pcgo_set_collision_shape(pair_caching_ghost_object* ghost_object,
  //				collision_shape* shape);

  // btKinematicCharacterController
  kinematic_character_controller* new_kinematic_character_controller
  (pair_caching_ghost_object* ghost_object,
   convex_shape* convex_shape,
   scalar step_height);
  void free_kinematic_character_controller(kinematic_character_controller* kcc);
  void set_up(kinematic_character_controller* kcc, scalar x, scalar y, scalar z);
  void get_up(const kinematic_character_controller* kcc, scalar* x, scalar* y, scalar* z);
  void set_angular_velocity(kinematic_character_controller* kcc,
			    scalar ang1,
			    scalar ang2,
			    scalar ang3);
  void get_angular_velocity(const kinematic_character_controller* kcc,
			    scalar* ang1,
			    scalar* ang2,
			    scalar* ang3);
  void set_linear_velocity(kinematic_character_controller* kcc,
			   scalar vx,
			   scalar vy,
			   scalar vz);
  void get_linear_velocity(kinematic_character_controller* kcc,
			   scalar* vx,
			   scalar* vy,
			   scalar* vz);
  void set_linear_damping(kinematic_character_controller* kcc, scalar d);
  scalar get_linear_damping(kinematic_character_controller* kcc);
  void set_angular_damping(kinematic_character_controller* kcc, scalar d);
  scalar get_angular_damping(kinematic_character_controller* kcc);
  void reset(kinematic_character_controller* kcc, collision_world* collision_world);
  void warp(kinematic_character_controller* kcc, scalar x, scalar y, scalar z);
  void set_step_height(kinematic_character_controller* kcc, scalar step_height);
  scalar get_step_height(kinematic_character_controller* kcc);
  void set_fall_speed(kinematic_character_controller* kcc, scalar fall_speed);
  scalar get_fall_speed(kinematic_character_controller* kcc);
  void set_jump_speed(kinematic_character_controller* kcc, scalar jump_speed);
  scalar get_jump_speed(kinematic_character_controller* kcc);
  void set_max_jump_height(kinematic_character_controller* kcc,
			   scalar max_jump_height);
  int can_jump(kinematic_character_controller* kcc); // returns bool
  void jump(kinematic_character_controller* kcc);
  void apply_impulse(kinematic_character_controller* kcc,
		     scalar ix,
		     scalar iy,
		     scalar iz);
  void kcc_set_gravity(kinematic_character_controller* kcc,
		       scalar gx,
		       scalar gy, 
		       scalar gz);
  void get_gravity(kinematic_character_controller* kcc,
		   scalar* gx,
		   scalar* gy,
		   scalar* gz);
  void set_max_slope(kinematic_character_controller* kcc, scalar slope_radians);
  scalar get_max_slope(kinematic_character_controller* kcc);
  void set_max_penetration_depth(kinematic_character_controller* kcc, scalar d);
  scalar get_max_penetration_depth(kinematic_character_controller* kcc);
  pair_caching_ghost_object* get_ghost_object(kinematic_character_controller* kcc);
  void set_use_ghost_sweep_test(kinematic_character_controller* kcc,
				int bool_use_ghost_object_sweep_test);
  int on_ground(kinematic_character_controller* kcc); // returns bool
  void set_up_interpolate(kinematic_character_controller* kcc, int bool_value); // ?
  action_interface* kinematic_character_controller_to_action_interface
  (kinematic_character_controller* kcc);

#ifdef __cplusplus
}
#endif

#endif
